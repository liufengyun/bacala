package bacala.maven

import bacala.util._
import bacala.core.{JLib, JPackage}

/** Fetches POM for a package
  */
class PomFetcher(base: String) extends Worker[JPackage, String] {
  override def apply(p: JPackage) = HttpFetcher.get(pomURL(p))

  /** Returns POM URL for a package
    *
    * Format: $BASE_REPO/:groupId/:artifactId/:version/:artifactId-version.pom
    *
    * e.g. http://repo1.maven.org/maven2/org/scala-lang/scala-library/2.11.6/scala-library-2.11.6.pom
    */
  def pomURL(p: JPackage) = {
    s"${base}/${p.groupId.replace(".", "/")}/${p.artifactId}/${p.version}/${p.artifactId}-${p.version}.pom"
  }
}


/** Fetches Meta file for an artifact
  */
class MetaFetcher(base: String) extends Worker[JLib, String] {
  /** Fetches the Meta file
    */
  override def apply(lib: JLib) = HttpFetcher.get(metaDataURL(lib))

  /** Returns the meta-data URL for a library
    *
    * Format:  $BASE_REPO/:groupId/:artifactId/maven-metadata.xml
    *
    * e.g.
    * http://repo1.maven.org/maven2/org/scala-lang/scala-library/maven-metadata.xml
    */
  def metaDataURL(lib: JLib) = {
    s"${base}/${lib.groupId.replace(".", "/")}/${lib.artifactId}/maven-metadata.xml"
  }
}
