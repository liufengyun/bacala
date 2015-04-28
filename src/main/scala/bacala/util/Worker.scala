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

/** Implements cached worker
  *
  * Cached worker always push the value to the first worker in the chain,
  * where the cache is located.
  */
trait CachedWorker[T, R] extends Worker[T, R] { outer =>
  def cache: Cache[T, Option[R]]
  def worker: Worker[T, R] = null // optional worker

  override def apply(p: T) = {
    if (worker == null ) cache.fetch(p, None)
    else cache.fetch(p, worker(p))
  }

  override def or(fallback: T => Option[R]) = new CachedWorker[T, R] {
    override val cache = outer.cache
    override val worker = outer

    private var noneKeys = Set[T]() // keys that return None from fallback

    override def apply(p: T) = outer(p) match {
      case None =>
        if (noneKeys.contains(p)) None else {
          val value = fallback(p)
          // push value to cache root
          if (value.nonEmpty) cache.update(p, value) else noneKeys = noneKeys + p
          value
        }
      case res => res
    }
  }
}

/** Provides memory-based cache for worker
  */
trait MemoryBase[T, R] { this: CachedWorker[T, R] =>
  val cache = new MemoryCache[T, Option[R]] {}
}
