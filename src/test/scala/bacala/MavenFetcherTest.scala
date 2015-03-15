package bacala.test

import scala.xml.XML
import org.scalatest._
import bacala.maven._

class MavenFetchSuite extends BasicSuite {

  test("generate correct package POM URL") {
    val p = MavenPackage("org.scala-lang", "scala-library", "2.11.5")
    assert(MavenFetcher.pomURL(p) ===
      "http://repo1.maven.org/maven2/org/scala-lang/scala-library/2.11.5/scala-library-2.11.5.pom"
    )
  }

  test("generate correct artifact meta-data XML URL") {
    assert(MavenFetcher.metaDataURL("org.scala-lang", "scala-library") ===
      "http://repo1.maven.org/maven2/org/scala-lang/scala-library/maven-metadata.xml"
    )
  }

  test("fetch POM file") {
    val p = MavenPackage("org.scala-lang", "scala-library", "2.11.5")
    val file = MavenFetcher(p)
    assert((XML.loadString(file) \ "artifactId").length === 1)
  }

  test("fetch meta data XML") {
    val file = MavenFetcher.getMetaData("org.scala-lang", "scala-library")
    assert((XML.loadString(file) \ "versioning" \ "versions").length > 0)
  }
}
