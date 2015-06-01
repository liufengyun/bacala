package bacala.maven

import bacala.util.FileCache
import java.io.File
import scala.io.Source

class PomCache(val baseDir: String) extends FileCache[MPackage, String] {
  val keyToPath = (pkg: MPackage) => new File(pkg.lib.groupId, pkg.lib.artifactId + "-" + pkg.version).toString + ".xml"

  val valueToString = (v: String) => v

  val stringToValue = (content: String) => content
}

class VersionsCache(val baseDir: String) extends FileCache[MLib, String] {
  val keyToPath = (lib: MLib) => new File(lib.groupId, lib.artifactId).toString + ".xml"
  val valueToString = (v: String) => v

  val stringToValue = (content: String) => content
}
