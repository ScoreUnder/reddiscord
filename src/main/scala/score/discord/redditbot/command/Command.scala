package score.discord.redditbot.command

import net.dv8tion.jda.core.entities.Message

trait Command {
  def name: String

  def canBeExecuted(message: Message): Boolean

  def execute(message: Message, args: String)
}
