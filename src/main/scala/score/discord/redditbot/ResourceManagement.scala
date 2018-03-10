package score.discord.redditbot

object ResourceManagement {
  def autoClose[T <: AutoCloseable, U](obj: T)(body: (T) => U): U = {
    try {
      body(obj)
    } finally {
      obj.close()
    }
  }
}
