package bacala.test.maven

import scala.xml.XML
import bacala.test._
import bacala.maven._

class MavenFetcherSuite extends BasicSuite {

  test("generate correct package POM URL") {
    val fetcher = new PomFetcher("http://www.test.org")
    val p = MavenPackage(MavenArtifact("org.scala-lang", "scala-library"), "2.11.5")
    assert(fetcher.pomURL(p) ===
      "http://www.test.org/org/scala-lang/scala-library/2.11.5/scala-library-2.11.5.pom"
    )
  }

  test("generate correct artifact meta-data XML URL") {
    val fetcher = new MetaFetcher("http://www.test.org")
    assert(fetcher.metaDataURL(MavenArtifact("org.scala-lang", "scala-library")) ===
      "http://www.test.org/org/scala-lang/scala-library/maven-metadata.xml"
    )
  }

  test("fetch POM file") {
    val p = MavenPackage(MavenArtifact("org.scala-lang", "scala-library"), "2.11.5")
    val file = Workers.DefaultPomFetcher(p)
    assert(file.nonEmpty)
    assert((XML.loadString(file.get) \ "artifactId").length === 1)
  }

  test("fetch meta data XML") {
    val file = Workers.DefaultMetaFetcher(MavenArtifact("org.scala-lang", "scala-library"))
    assert(file.nonEmpty)
    assert((XML.loadString(file.get) \ "versioning" \ "versions").length > 0)
  }

  test("fetch in-existent file") {
    val file = Workers.DefaultMetaFetcher(MavenArtifact("org.scala-lang", "foo-library"))
    assert(file.isEmpty)
  }

}
