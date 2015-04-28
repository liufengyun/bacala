package bacala.util

import scala.collection.mutable.{Map => MMap}
import bacala.util.ConsoleHelper.ColorText

/** Measures time cost of a group of expressions
  */
class Measure {
  private var pairs: Seq[(String, Long)] = Seq()

  def time[A](name: String)(a: => A) = {
    val now = System.nanoTime
    val result = a

    val micros = (System.nanoTime - now) / 1000
    pairs = pairs :+ (name -> micros)

    result
  }

  override def toString = {
    val formatter = java.text.NumberFormat.getInstance()
    "\n================ Performance(microseconds) ================\n".bold +
      (pairs :\ "") { case ((name, time), acc) =>
        s"$name\t\t" + formatter.format(time) + "\n" + acc
    }
  }
}
