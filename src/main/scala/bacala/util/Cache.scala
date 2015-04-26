package bacala.util

import scala.collection.concurrent.TrieMap

/** Thread-safe cache logic
  */

trait Cache[K, V] {
  def fetch(k: K, f: => V): V
  def update(k: K, v: V): Unit
}

/** Cache in memory
  */
trait MemoryCache[K, V] extends Cache[K, V] {
  private val cache = new TrieMap[K, V]

  override def fetch(k: K, f: => V): V = cache.getOrElseUpdate(k, f)

  override def update(k: K, v: V): Unit = cache.put(k, v)
}
