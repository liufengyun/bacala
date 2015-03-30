package bacala.util

import scala.collection.concurrent.TrieMap

/**
  * Thread-safe cache logic
  */

trait Cache[K, V] {
  private val cache = new TrieMap[K, V]

  def fetch(k: K, f: => V) = cache.getOrElseUpdate(k, f)
}
