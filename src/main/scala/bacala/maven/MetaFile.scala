package bacala.maven

import scala.xml.XML
import scala.xml.Node

/**
  * The object MetaFile is the interface to get all version definitions for
  * an artifact. It does caching internally.
  */
object MetaFile {
  // TODO : thread-safe
  var cache = Map[String, Seq[String]]()

  def apply(groupId: String, artifactId: String): Option[Seq[String]] = {
    if (cache.contains(groupId + artifactId)) Some(cache(groupId + artifactId)) else

    MavenFetcher.getMetaData(groupId, artifactId) map { metaData =>
      val versions = (XML.loadString(metaData) \ "versioning" \ "versions" \ "version").map(_.text.trim).filter { ver =>
        // filter old, illegal version numbers
        if (Version.unapply(ver).nonEmpty) true else {
          println("Error: unknown version format " + ver + " in meta data XML of " +
            groupId + ":" +artifactId)
          false
        }
      }
      cache = cache + ((groupId + artifactId) -> versions)

      versions
    }
  }
}
