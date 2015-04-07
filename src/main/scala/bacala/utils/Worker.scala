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
trait CachedWorker[T, R] extends Worker[T, R] {
  def worker: Worker[T, R]
  def cache: Cache[T, Option[R]]

  override def apply(p: T) = cache.fetch(p, worker(p))

  override def or(fallback: T => Option[R]) = new CachedWorker[T, R] {
    override val cache = CachedWorker.this.cache
    override val worker = CachedWorker.this

    override def apply(p: T) = worker(p) match {
      case None => cache.fetch(p, fallback(p))
      case res => res
    }
  }
}
