package score.discord.redditbot

import java.util.concurrent.Executors

import net.dv8tion.jda.core.entities.{Message, MessageChannel, MessageType}
import net.dv8tion.jda.core.events.message.react.{GenericMessageReactionEvent, MessageReactionAddEvent, MessageReactionRemoveAllEvent}
import net.dv8tion.jda.core.events.message.{MessageDeleteEvent, MessageReceivedEvent, MessageUpdateEvent}
import net.dv8tion.jda.core.events.{Event, ReadyEvent}
import net.dv8tion.jda.core.hooks.EventListener
import score.discord.redditbot.DiscordUtil._
import score.discord.redditbot.FutureUtil._
import score.discord.redditbot.UIConstants.{ERROR_EMOJI, OK_EMOJI}
import score.discord.redditbot.command.PinCommand

import scala.async.Async._
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

class RedditEngine extends EventListener {
  private val pinCache = mutable.HashMap[ChanID, mutable.Set[MesgID]]()
  private implicit val executor: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  val voteCache = new RedditVoteCache
  val PINS_MAX = 50
  val UPVOTE = "⬆"
  val DOWNVOTE = "⬇"
  val CONFIDENCE_UP = 1.45
  val CONFIDENCE_DOWN = 1.7

  class RedditModeEnable extends PinCommand {
    override def name: String = "redditon"

    override def execute(message: Message, args: String): Unit = {
      async {
        val pins = await(message.getChannel.getPinnedMessages.queueFuture())
        pinCache(message.getChannel.getIdLong) = mutable.TreeSet(pins.asScala.map(_.getIdLong): _*)
        message.addReaction(OK_EMOJI).queue()
      }.failed.foreach { err =>
        System.err.println("Failed to turn reddit mode on")
        err.printStackTrace()
        message.addReaction(ERROR_EMOJI).queue()
      }
    }
  }

  class RedditModeDisable extends PinCommand {
    override def name: String = "redditoff"

    override def execute(message: Message, args: String): Unit = {
      async {
        pinCache.remove(message.getChannel.getIdLong)
        message.addReaction(OK_EMOJI).queue()
      }.failed.foreach { err =>
        System.err.println("Failed to turn reddit mode off")
        err.printStackTrace()
        message.addReaction(ERROR_EMOJI).queue()
      }
    }
  }

  def isRedditMode(channel: ChanID): Boolean = pinCache contains channel

  def addRedditReacts(message: Message): Unit = {
    message.addReaction(UPVOTE).queue()
    message.addReaction(DOWNVOTE).queue()
  }

  def cacheVotes(channel: MessageChannel, mesgID: MesgID): Future[Option[voteCache.Votes]] = {
    async {
      voteCache(mesgID) match {
        case s@Some(_) => s
        case None =>
          await(channel.getMessageById(mesgID).queueFuture().asSuccessOption()) match {
            case Some(msg) =>
              val reacts = msg.getReactions.asScala
              val upvotes = reacts.find(_.getReactionEmote.getName == UPVOTE)
              val downvotes = reacts.find(_.getReactionEmote.getName == DOWNVOTE)
              val votes = voteCache.set(mesgID,
                up = upvotes.map(_.getCount - 1).getOrElse(0),
                down = downvotes.map(_.getCount - 1).getOrElse(0))
              if (upvotes.isDefined && downvotes.isDefined && !votes.botVoted) {
                val users = await(upvotes.get.getUsers.queueFuture()).asScala
                if (users.exists(_.getIdLong == channel.getJDA.getSelfUser.getIdLong))
                  votes.botVoted = true
              }
              Some(votes)
            case None => None
          }
      }
    }
  }

  def attemptPin(channel: MessageChannel, msgId: MesgID, cilbUp: Double): Future[Unit] = {
    async {
      val pins = pinCache(channel.getIdLong)
      if (!pins.contains(msgId)) {
        if (pins.size >= PINS_MAX) {
          val pinConfidences = await(Future.sequence(pins.view.map { pin =>
            cacheVotes(channel, pin).map(votes => (votes, pin))
          }.toVector)).collect {
            case (Some(votes), pin) if votes.botVoted =>
              (MathUtil.ciLowerBound(votes.up, votes.down, CONFIDENCE_UP), pin)
          }
          for ((worstCilb, worstPin) <- pinConfidences.sortBy(_._1).headOption) {
            if (worstCilb < cilbUp) {
              for {
                pin <- channel.getMessageById(worstPin).queueFuture()
                _ <- pin.unpin.queueFuture()
              } {
                channel.pinMessageById(msgId).queue()
              }
            }
          }
        } else {
          channel.pinMessageById(msgId).queue()
        }
      }
    }
  }

  def checkVotes(channel: MessageChannel, msgId: MesgID, votes: voteCache.Votes): Unit = {
    if (!votes.botVoted) return
    if (votes.up > votes.down) {
      val cilbUp = MathUtil.ciLowerBound(votes.up, votes.down, CONFIDENCE_UP)
      if (cilbUp > 0.5)
        attemptPin(channel, msgId, cilbUp)
    } else {
      val cilbDown = MathUtil.ciLowerBound(votes.down, votes.up, CONFIDENCE_DOWN)
      if (cilbDown > 0.5)
        channel.deleteMessageById(msgId).reason("downvoted").queue()
    }
  }

  override def onEvent(event: Event): Unit = event match {
    case _: ReadyEvent |
         _: MessageReceivedEvent |
         _: GenericMessageReactionEvent |
         _: MessageUpdateEvent |
         _: MessageReactionRemoveAllEvent |
         _: MessageDeleteEvent =>
      Future {
        event match {
          case ev: ReadyEvent =>
          // TODO: Load reddit channels from save file
          case ev: MessageReceivedEvent if isRedditMode(ev.getChannel.getIdLong) =>
            if (ev.getMessage.getType == MessageType.DEFAULT) {
              voteCache.set(ev.getMessageIdLong, 0, 0).botVoted = true
              addRedditReacts(ev.getMessage)
            }
          case ev: GenericMessageReactionEvent if isRedditMode(ev.getChannel.getIdLong) =>
            val channel = ev.getChannel
            val reaction = ev.getReaction
            val message = ev.getReaction.getMessageIdLong
            val dir = if (ev.isInstanceOf[MessageReactionAddEvent]) 1 else -1
            reaction.getReactionEmote.getName match {
              case UPVOTE =>
                if (voteCache(message).isDefined) {
                  val votes = voteCache.upvote(message, dir)
                  checkVotes(channel, message, votes)
                } else cacheAndCheckVotes(channel, message)
              case DOWNVOTE =>
                if (voteCache(message).isDefined) {
                  val votes = voteCache.downvote(message, dir)
                  checkVotes(channel, message, votes)
                } else cacheAndCheckVotes(channel, message)
              case _ =>
            }
          case ev: MessageUpdateEvent if isRedditMode(ev.getChannel.getIdLong) =>
            val pins = pinCache(ev.getChannel.getIdLong)
            if (ev.getMessage.isPinned)
              pins += ev.getMessageIdLong
            else
              pins -= ev.getMessageIdLong
          case ev: MessageReactionRemoveAllEvent =>
            voteCache.invalidate(ev.getMessageIdLong)
          case ev: MessageDeleteEvent =>
            voteCache.invalidate(ev.getMessageIdLong)
            pinCache.get(ev.getChannel.getIdLong).foreach(_ -= ev.getMessageIdLong)
          case _ =>
        }
      }.failed.foreach(_.printStackTrace())
    case _ =>
  }

  private def cacheAndCheckVotes(channel: MessageChannel, message: MesgID): Unit = {
    val cacheFuture = cacheVotes(channel, message)
    cacheFuture.failed.foreach(_.printStackTrace())
    cacheFuture.foreach {
      case Some(votes) =>
        checkVotes(channel, message, votes)
      case None =>
    }
  }
}
