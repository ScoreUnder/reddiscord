package score.discord.redditbot

import java.util

class CacheMap[K, V](maxSize: Int) extends util.LinkedHashMap[K, V](16, 0.75f, true) {
  override def removeEldestEntry(eldest: util.Map.Entry[K, V]): Boolean =
    size >= maxSize
}
