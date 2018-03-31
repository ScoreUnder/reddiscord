package score.discord.redditbot

import java.util.concurrent.{Callable, Executors, TimeUnit}

import net.dv8tion.jda.core.entities.MessageType
import net.dv8tion.jda.core.events.Event
import net.dv8tion.jda.core.events.message.MessageReceivedEvent
import net.dv8tion.jda.core.hooks.EventListener

class HidePinMessageListener extends EventListener {
  override def onEvent(event: Event): Unit = event match {
    case ev: MessageReceivedEvent =>
      if (ev.getMessage.getType == MessageType.CHANNEL_PINNED_ADD
        && ev.getAuthor.getIdLong == ev.getJDA.getSelfUser.getIdLong) {
          ev.getMessage.delete().queueAfter(10, TimeUnit.SECONDS)
      }
    case _ =>
  }
}
