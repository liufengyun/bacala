package bacala.util

import scala.io.Source
import java.io.File
import java.io.FileWriter

/** Cache in file system
  */
trait FileCache[K, V] extends Cache[K, V] {
  import IOHelper._

  def baseDir: String
  def keyToPath: K => String
  def valueToString: V => String
  def stringToValue: String => V

  override def exists(k: K): Boolean = {
    new File(baseDir, keyToPath(k)).exists
  }

  override def fetch(k: K): V = {
    val file = new File(baseDir, keyToPath(k))
    stringToValue(Source.fromFile(file.toString).mkString)
  }

  override def update(k: K, v: V): Unit = {
    val file = new File(baseDir, keyToPath(k))
    file.getParentFile().mkdirs()
    use(new FileWriter(file)) { f => f.write(valueToString(v)) }
  }
}
