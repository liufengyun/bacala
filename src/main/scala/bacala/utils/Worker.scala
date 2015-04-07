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
  */
trait CachedWorker[T, R] extends Worker[T, R] { outer =>
  def worker: Worker[T, R]
  def cache: Cache[T, Option[R]]

  override def apply(p: T) = cache.fetch(p, worker(p))

  override def or(fallback: T => Option[R]) = new CachedWorker[T, R] {
    override val cache = outer.cache
    override val worker = outer
    var noneKeys = Set[T]() // keys that return None from fallback

    override def apply(p: T) = worker(p) match {
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
