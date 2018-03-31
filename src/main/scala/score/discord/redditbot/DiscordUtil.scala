package score.discord.redditbot

import net.dv8tion.jda.core.requests.RestAction

import scala.concurrent.{Future, Promise}

object DiscordUtil {
  type ChanID = Long
  type MesgID = Long
  type UserID = Long

  def parseID(id: String): Long =
    try {
      id.toLong
    } catch {
      case _: NumberFormatException =>
        java.lang.Long.parseUnsignedLong(id)
    }

  implicit class RichRestAction[T](val me: RestAction[T]) extends AnyVal {
    def queueFuture(): Future[T] = {
      val promise = Promise[T]
      me.queue(promise.success _, promise.failure _)
      promise.future
    }
  }
}
