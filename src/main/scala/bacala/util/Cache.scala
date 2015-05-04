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
  private val store = new TrieMap[K, V]

  override def fetch(k: K, f: => V): V = store.getOrElseUpdate(k, f)

  override def update(k: K, v: V): Unit = store.put(k, v)
}
