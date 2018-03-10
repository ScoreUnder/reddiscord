package score.discord.redditbot.command

import net.dv8tion.jda.core.entities.Message
import score.discord.redditbot.UIConstants.{ERROR_EMOJI, OK_EMOJI}

import scala.async.Async._
import scala.concurrent.ExecutionContext.Implicits.global

class PinSaveCommand extends PinCommand {
  def name = "savepins"

  override def execute(message: Message, args: String): Unit = {
    async {
      await(PinCommand.autosavePins(message.getTextChannel, args.replaceAll("[\0/]", "")))
      message.addReaction(OK_EMOJI).queue()
    }.failed.foreach({ f =>
      System.err.println("Failed to save pins")
      f.printStackTrace()
      message.addReaction(ERROR_EMOJI).queue()
    })
  }
}
