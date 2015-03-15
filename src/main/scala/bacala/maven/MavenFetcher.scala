package bacala.maven

/*
 * Fetch the POM file of a package from repository
 *
 * TODO: (1) multiple repos; (2) retries logic; (3) checksum;
 */

import scalaj.http._

object MavenFetcher extends (MavenPackage => String) {
  val MavenRepoBase = "http://repo1.maven.org/maven2"

  override def apply(p: MavenPackage) = getResponse(pomURL(p))

  def getMetaData(groupId: String, artifactId: String) = getResponse(metaDataURL(groupId, artifactId))

  def getResponse(url: String) = {
    Http(url).asString.body
  }

  /*
   * returns POM URL for a package
   *
   * Format: $BASE_REPO/:groupId/:artifactId/:version/:artifactId-version.pom
   *
   * e.g. http://repo1.maven.org/maven2/org/scala-lang/scala-library/2.11.6/scala-library-2.11.6.pom
   */
  def pomURL(p: MavenPackage) = {
    s"${MavenRepoBase}/${p.groupId.replace(".", "/")}/${p.artifactId}/${p.version}/${p.artifactId}-${p.version}.pom"
  }

  /* returns the meta-data URL for an artifact
   *
   * Format:  $BASE_REPO/:groupId/:artifactId/maven-metadata.xml
   *
   * e.g.
   * http://repo1.maven.org/maven2/org/scala-lang/scala-library/maven-metadata.xml
   */
  def metaDataURL(groupId: String, artifactId: String) = {
    s"${MavenRepoBase}/${groupId.replace(".", "/")}/${artifactId}/maven-metadata.xml"
  }
}
