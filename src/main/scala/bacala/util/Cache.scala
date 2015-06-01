package bacala.util

import scala.collection.concurrent.TrieMap

/** Thread-safe cache logic
  */

trait Cache[K, V] {
  def exists(k: K): Boolean
  def fetch(k: K): V
  def update(k: K, v: V): Unit

  def fetch(k: K, f: => V): V = {
    if (exists(k)) {
      fetch(k)
    } else {
      val value = f
      update(k, value)
      value
    }
  }
}

/** Cache in memory
  */
trait MemoryCache[K, V] extends Cache[K, V] {
  private val store = new TrieMap[K, V]

  override def exists(k: K): Boolean = store.contains(k)

  override def fetch(k: K): V = store(k)

  override def update(k: K, v: V): Unit = store.put(k, v)
}
