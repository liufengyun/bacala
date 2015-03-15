package bacala.maven

/*
 * Fetch the POM file of a package from repository
 *
 * The POM URL for a package:
 *   $BASE_REPO/:groupId/:artifactId/:version/:artifactId-version.pom
 *
 *   e.g. http://repo1.maven.org/maven2/org/scala-lang/scala-library/2.11.6/scala-library-2.11.6.pom
 *
 * The meta-data URL for all versions of a module:
 *   $BASE_REPO/:groupId/:artifactId/maven-metadata.xml
 *
 *   e.g.
 *   http://repo1.maven.org/maven2/org/scala-lang/scala-library/maven-metadata.xml
 *
 * TODO: (1) multiple repos; (2) retries logic; (3) checksum;
 */
class MavenFetcher extends (MavenPackage => String) {
  val MavenRepoBase = "http://repo1.maven.org/maven2"

  override def apply(p: MavenPackage) = ???
}
