package bacala.maven

import bacala.util._

/** Fetches POM for a package
  */
class PomFetcher(base: String) extends Worker[MPackage, String] {
  override def apply(p: MPackage) = HttpFetcher.get(pomURL(p))

  /** Returns POM URL for a package
    *
    * Format: $BASE_REPO/:groupId/:artifactId/:version/:artifactId-version.pom
    *
    * e.g. http://repo1.maven.org/maven2/org/scala-lang/scala-library/2.11.6/scala-library-2.11.6.pom
    */
  def pomURL(p: MPackage) = {
    s"${base}/${p.groupId.replace(".", "/")}/${p.artifactId}/${p.version}/${p.artifactId}-${p.version}.pom"
  }
}


/** Fetches Meta file for an artifact
  */
class MetaFetcher(base: String) extends Worker[MLib, String] {
  /** Fetches the Meta file
    */
  override def apply(lib: MLib) = HttpFetcher.get(metaDataURL(lib))

  /** Returns the meta-data URL for a library
    *
    * Format:  $BASE_REPO/:groupId/:artifactId/maven-metadata.xml
    *
    * e.g.
    * http://repo1.maven.org/maven2/org/scala-lang/scala-library/maven-metadata.xml
    */
  def metaDataURL(lib: MLib) = {
    s"${base}/${lib.groupId.replace(".", "/")}/${lib.artifactId}/maven-metadata.xml"
  }
}
