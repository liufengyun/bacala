package bacala.maven

import scala.xml.XML
import scala.xml.Node
import bacala.util.Cache

/**
  * The object MetaFile is the interface to get all version definitions for
  * an artifact. It does caching internally.
  */
object MetaFileParser extends (String => Iterable[String]) {
  override def apply(metaData: String) = {
    (XML.loadString(metaData) \ "versioning" \ "versions" \ "version").
      map(_.text.trim).
      filter(ver =>
        // filter old, illegal version numbers
        if (Version.unapply(ver).nonEmpty) true else {
          println("Error: unknown version format " + ver + " in meta data XML" )
          false
        }
      )
  }
}
