package score.discord.redditbot.command

import net.dv8tion.jda.core.events.Event
import net.dv8tion.jda.core.events.message.MessageReceivedEvent
import net.dv8tion.jda.core.hooks.EventListener


class CommandExecutor extends EventListener {
  private[this] var _commands: Map[String, Command] = Map.empty
  var prefix = "/reddit:"

  def register(cmd: Command): Unit = {
    _commands += cmd.name -> cmd
  }

  override def onEvent(event: Event): Unit = {
    event match {
      case ev: MessageReceivedEvent =>
        if (ev.getAuthor.isBot) return

        val rawTxt = ev.getMessage.getContentRaw
        if (!rawTxt.startsWith(prefix)) return

        val Array(cmdStr, args) =
          rawTxt.split("\\s", 2) match {
            case Array(s) => Array(s, "")
            case a => a
          }

        _commands.get(cmdStr.substring(prefix.size)) match {
          case None => ev.getMessage.addReaction("❔").queue()
          case Some(cmd) =>
            if (cmd.canBeExecuted(ev.getMessage))
              cmd.execute(ev.getMessage, args.trim)
            else
              ev.getMessage.addReaction("🔒").queue()
        }
      case _ =>
    }
  }
}
