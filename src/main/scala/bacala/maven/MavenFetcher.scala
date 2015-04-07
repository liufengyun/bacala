package bacala.maven

import bacala.util._

/** Fetches POM for a package
  */
class PomFetcher(base: String) extends Worker[MavenPackage, String] {
  override def apply(p: MavenPackage) = HttpFetcher.get(pomURL(p))

  /** Returns POM URL for a package
    *
    * Format: $BASE_REPO/:groupId/:artifactId/:version/:artifactId-version.pom
    *
    * e.g. http://repo1.maven.org/maven2/org/scala-lang/scala-library/2.11.6/scala-library-2.11.6.pom
    */
  def pomURL(p: MavenPackage) = {
    s"${base}/${p.groupId.replace(".", "/")}/${p.artifactId}/${p.version}/${p.artifactId}-${p.version}.pom"
  }
}


/** Fetches Meta file for an artifact
  */
class MetaFetcher(base: String) extends Worker[MavenArtifact, String] {
  /** Fetches the Meta file
    */
  override def apply(artifact: MavenArtifact) = HttpFetcher.get(metaDataURL(artifact))

  /** Returns the meta-data URL for an artifact
    *
    * Format:  $BASE_REPO/:groupId/:artifactId/maven-metadata.xml
    *
    * e.g.
    * http://repo1.maven.org/maven2/org/scala-lang/scala-library/maven-metadata.xml
    */
  def metaDataURL(artifact: MavenArtifact) = {
    s"${base}/${artifact.groupId.replace(".", "/")}/${artifact.artifactId}/maven-metadata.xml"
  }
}
