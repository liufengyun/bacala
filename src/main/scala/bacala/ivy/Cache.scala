package bacala.ivy

import bacala.util.FileCache
import java.io.File
import scala.io.Source

class IvyCache(val baseDir: String, parser: String => IDescriptor) extends FileCache[IPackage, IDescriptor] {
  val keyToPath = (pkg: IPackage) => new File(pkg.lib.groupId, pkg.lib.name + "-" + pkg.version).toString + ".xml"

  val valueToString = (descriptor: IDescriptor) => {
    val temp = File.createTempFile("bacala-ivy-cache-temp", ".ivy")
    temp.deleteOnExit
    descriptor.md.toIvyFile(temp)
    Source.fromFile(temp.toString).mkString
  }

  val stringToValue = content => parser(content)
}

class VersionsCache(val baseDir: String) extends FileCache[ILib, Seq[String]] {
  val keyToPath = (lib: ILib) => lib.groupId + ".txt"
  val valueToString = (v: Seq[String]) => v.mkString("\n")
  val stringToValue = (str: String) => str.split("\n").toSeq
}
