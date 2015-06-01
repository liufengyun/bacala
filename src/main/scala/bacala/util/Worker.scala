package bacala.util

/** Implements chainable workers
  */
trait Worker[T, R] extends (T => Option[R]) {
  def or(fallback: T => Option[R]) = new Worker[T, R] {
    override def apply(p: T) = Worker.this(p) match {
      case None => fallback(p)
      case res => res
    }
  }
}

/** Creates worker from a function
  */
object Worker {
  def apply[T, R](f: T => Option[R]): Worker[T, R] = new Worker[T, R] {
    override def apply(t: T): Option[R] = f(t)
  }
}

/** Implements cached worker
  *
  * Cached worker always push the value to the first worker in the chain,
  * where the cache is located.
  */
abstract class CachedWorker[T, R] extends Worker[T, R] { outer =>
  def cache: Cache[T, R]

  override def apply(p: T) = {
    if (cache.exists(p))
      Some(cache.fetch(p))
    else  None
  }

  override def or(fallback: T => Option[R]) = new CachedWorker[T, R] {
    override val cache = outer.cache

    override def apply(p: T) = {
      if (cache.exists(p))
        Some(cache.fetch(p))
      else {
        val value = fallback(p)
        if (!value.isEmpty) cache.update(p, value.get)
        value
      }
    }
  }
}

/** Defines a cache base
  */
trait CacheBase[T, R] {
  def cache: Cache[T, R]
}

/** Provides memory-based cache for worker
  */
trait MemoryBase[T, R] extends CacheBase[T, R] {
  val cache = new MemoryCache[T, R] {}
}
