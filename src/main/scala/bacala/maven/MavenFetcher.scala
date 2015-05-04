package bacala.maven

import bacala.util._

trait Resolver { outer =>
  def resolveDescriptor(p: MPackage): Option[String]
  def resolveVersions(lib: MLib): Option[String]

  def or(fallback: Resolver) = new Resolver {
    def resolveDescriptor(p: MPackage) = {
      (Worker(outer.resolveDescriptor _) or (fallback.resolveDescriptor _))(p)
    }

    def resolveVersions(lib: MLib) = {
      (Worker(outer.resolveVersions _) or (fallback.resolveVersions _))(lib)
    }
  }
}

abstract class CachedResolver(resolver: Resolver) extends Resolver {
  def descriptorCache: Cache[MPackage, Option[String]]
  def versionsCache: Cache[MLib, Option[String]]

  def resolveDescriptor(p: MPackage): Option[String] =
    descriptorCache.fetch(p, resolver.resolveDescriptor(p))

  def resolveVersions(lib: MLib): Option[String] =
    versionsCache.fetch(lib, resolver.resolveVersions(lib))
}

/** Resolves descriptor or versions
  */
class MavenResolver(base: String) extends Resolver {
  override def resolveDescriptor(p: MPackage) = HttpFetcher.get(pomURL(p))

  /** Returns POM URL for a package
    *
    * Format: $BASE_REPO/:groupId/:artifactId/:version/:artifactId-version.pom
    *
    * e.g. http://repo1.maven.org/maven2/org/scala-lang/scala-library/2.11.6/scala-library-2.11.6.pom
    */
  def pomURL(p: MPackage) = {
    s"${base}/${p.groupId.replace(".", "/")}/${p.artifactId}/${p.version}/${p.artifactId}-${p.version}.pom"
  }

  /** Fetches the Meta file
    */
  override def resolveVersions(lib: MLib) = HttpFetcher.get(metaDataURL(lib))

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
