package score.discord.redditbot

import java.util

class RedditVoteCache {
  val MAX_CACHE_SIZE = 20000

  case class Votes(up: Int = 0, down: Int = 0, var botVoted: Boolean = false)

  private[this] val cache: util.LinkedHashMap[Long, Votes] =
    new util.LinkedHashMap[Long, Votes](16, 0.75f, true) {
      override def removeEldestEntry(eldest: util.Map.Entry[Long, Votes]): Boolean =
        size >= MAX_CACHE_SIZE
    }

  def apply(message: Long) = Option(cache.get(message))

  def update(message: Long, votes: Votes): Votes = cache.put(message, votes)

  def set(message: Long, up: Int, down: Int): Votes = {
    val newVotes = this(message) match {
      case None => Votes(up = up, down = down)
      case Some(votes) => votes.copy(up = up, down = down)
    }
    this(message) = newVotes
    newVotes
  }

  def upvote(message: Long, num: Int): Votes = {
    val votes = cache.get(message) ensuring (_ ne null)
    val newVotes = votes.copy(up = votes.up + num)
    cache.put(message, newVotes)
    newVotes
  }

  def downvote(message: Long, num: Int): Votes = {
    val votes = cache.get(message) ensuring (_ ne null)
    val newVotes = votes.copy(down = votes.down + num)
    cache.put(message, newVotes)
    newVotes
  }

  def invalidate(message: Long): Unit = cache.remove(message)
}
