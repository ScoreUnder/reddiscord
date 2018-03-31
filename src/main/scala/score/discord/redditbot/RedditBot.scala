package score.discord.redditbot

import net.dv8tion.jda.core.{AccountType, JDA, JDABuilder}
import score.discord.redditbot.command._

object RedditBot extends App {
  new RedditBot().start()
}

class RedditBot {
  var discord: Option[JDA] = None

  def start(): Unit = {
    discord match {
      case None =>
        val builder = new JDABuilder(AccountType.BOT)
        val token = io.Source.fromFile("token.txt").mkString.trim
        builder.setToken(token)
        val cmds = new CommandExecutor
        cmds register new PinLoadCommand
        cmds register new PinSaveCommand
        cmds register new PinResetCommand
        val reddit = new RedditEngine
        cmds register new reddit.RedditModeDisable
        cmds register new reddit.RedditModeEnable
        cmds register new reddit.RedditKarma
        builder addEventListener reddit
        builder addEventListener cmds
        builder addEventListener new HidePinMessageListener
        discord = Some(builder.buildBlocking())
      case Some(_) =>
        throw new UnsupportedOperationException("Already started, can't start again")
    }
  }

  def stop(): Unit = {
    discord match {
      case None =>
        throw new UnsupportedOperationException("Already stopped, can't stop again")
      case Some(bot) =>
        bot.shutdown()
        discord = None
    }
  }
}
