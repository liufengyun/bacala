package bacala.util

object IOHelper {
  def use[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
    try { f(param) } finally { param.close() }
}
