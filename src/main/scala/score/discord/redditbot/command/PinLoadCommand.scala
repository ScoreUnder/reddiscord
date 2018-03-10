package score.discord.redditbot.command

import java.io.File

import net.dv8tion.jda.core.MessageBuilder
import net.dv8tion.jda.core.entities.Message
import score.discord.redditbot.DiscordUtil._
import score.discord.redditbot.PinStorage
import score.discord.redditbot.UIConstants.{ERROR_EMOJI, OK_EMOJI}

import scala.async.Async._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class PinLoadCommand extends PinCommand {
  def name = "loadpins"

  override def execute(message: Message, args: String): Unit = {
    async {
      val channel = message.getTextChannel
      val servId = channel.getGuild.getIdLong
      val chanId = channel.getIdLong

      args.trim match {
        case "" =>
          await(message.getChannel.sendMessage(new MessageBuilder(
            s"Saved pins:\n`${getSavedPinFiles(servId, chanId).mkString("`, `")}`"
          ).build()).queueFuture())
        case date =>
          val pinStore = new PinStorage()
          if (args.contains('\0') || args.contains('/'))
            throw new IllegalArgumentException("No slash or null allowed in pin date")

          pinStore.loadPins(new File(PinCommand.PIN_DIR, s"pins.$servId.$chanId.$date.txt"))
          val pins = pinStore(chanId).get

          def reportNonfatal(msg: String)(result: Try[Void]) =
            result match {
              case Failure(ex) =>
                System.err.println(msg)
                ex.printStackTrace()
                channel.sendMessage(msg).queue()
                Success(null)
              case s: Success[_] => s
            }

          val unpins = PinCommand.clearPins(channel)

          val failedUnpins = for ((pin, Some(err)) <- await(unpins)) yield {
            err.printStackTrace()
            pin.getId
          }
          if (failedUnpins.nonEmpty)
            channel.sendMessage(s"Failed to unpin ${failedUnpins.mkString(", ")}").queue()

          val pinRemaining = for (pin <- pins.reverse)
            yield channel.pinMessageById(pin).queueFuture().transform(reportNonfatal(s"Failed to pin $pin"))
          await(Future.sequence(pinRemaining))

          message.addReaction(OK_EMOJI).queue()
      }
    }.failed.foreach({ f =>
      System.err.println("Failed to save pins")
      f.printStackTrace()
      message.addReaction(ERROR_EMOJI).queue()
    })
  }

  def getSavedPinFiles(servId: Long, chanId: Long): Seq[String] = {
    val regex = s"^pins\\.$servId\\.$chanId\\.(.+)\\.txt$$".r
    PinCommand.PIN_DIR
      .listFiles().toList.view
      .flatMap((file: File) => regex.findFirstMatchIn(file.getName))
      .map(_.group(1))
      .sorted
  }
}
