package score.discord.redditbot.command

import net.dv8tion.jda.core.entities.Message
import score.discord.redditbot.UIConstants.{ERROR_EMOJI, OK_EMOJI}

import scala.async.Async._
import scala.concurrent.ExecutionContext.Implicits.global

class PinResetCommand extends PinCommand {
  override def name = "resetpins"

  override def execute(message: Message, args: String) {
    async {
      await(PinCommand.autosavePins(message.getTextChannel, "pre-reset"))
      val unpins = await(PinCommand.clearPins(message.getChannel))
      val failedUnpins = for ((pin, Some(err)) <- unpins) yield {
        err.printStackTrace()
        pin.getId
      }
      if (failedUnpins.nonEmpty)
        message.getChannel.sendMessage(s"Failed to unpin ${failedUnpins.mkString(", ")}").queue()
      message.addReaction(OK_EMOJI).queue()
    }.failed.foreach { ex =>
      System.err.println("Failed to reset pins")
      ex.printStackTrace()
      message.addReaction(ERROR_EMOJI).queue()
    }
  }
}
