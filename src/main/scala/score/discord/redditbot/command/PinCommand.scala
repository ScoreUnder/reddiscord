package score.discord.redditbot.command

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import net.dv8tion.jda.core.Permission
import net.dv8tion.jda.core.entities.{Message, MessageChannel, TextChannel}
import score.discord.redditbot.DiscordUtil._
import score.discord.redditbot.PinStorage

import scala.async.Async._
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Success

trait PinCommand extends Command {
  override def canBeExecuted(message: Message): Boolean =
    Option(message.getMember)
      .exists(_.hasPermission(message.getTextChannel, Permission.MESSAGE_MANAGE))
}

object PinCommand {
  val PIN_DIR = new File("pins")

  def clearPins(channel: MessageChannel): Future[Seq[(Message, Option[Throwable])]] = {
    async {
      val oldPins = await(channel.getPinnedMessages.queueFuture()).asScala
      val unpinRemaining = for (pin <- oldPins)
        yield pin.unpin.queueFuture().transform { t =>
          Success(pin -> t.failed.toOption)
        }
      await(Future.sequence(unpinRemaining))
    }
  }

  def autosavePins(channel: TextChannel, extra: String = "autosave"): Future[Unit] = {
    async {
      PIN_DIR.mkdir()
      val pinStore = new PinStorage()
      val pins = await(channel.getPinnedMessages.queueFuture())
      val servId = channel.getGuild.getIdLong
      val chanId = channel.getIdLong
      val date = new SimpleDateFormat("yyyy-MM-dd-HH:mm:ssZ").format(new Date())
      val extraWithDot = if (extra.nonEmpty) s".$extra" else ""
      pinStore(chanId) = pins.asScala.map(_.getIdLong)
      pinStore.savePins(new File(PIN_DIR, s"pins.$servId.$chanId.$date$extraWithDot.txt"))
    }
  }
}
