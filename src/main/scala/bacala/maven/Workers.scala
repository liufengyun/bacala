package bacala.maven

import bacala.core._
import bacala.util._

/** Workers factory
  */
object Workers {

  val mavenMainBase = "http://repo1.maven.org/maven2"

  /** Fetches POM file from the Maven main repository
    */
  val DefaultResolver = Resolvers(mavenMainBase)

  /** Cache and reuses the POM fetchers
    */
  object Resolvers extends MemoryCache[String, Resolver] {
    def apply(url: String) = fetch(url,
      new CachedResolver(new MavenResolver(url)) {
        val descriptorCache = new MemoryCache[MPackage, Option[String]] {}
        val versionsCache = new MemoryCache[MLib, Option[String]] {}
      })
  }

  /** Creates a chain of resolvers
    */
  def chainResolvers(initial: Resolver)(resolvers: Iterable[MResolver]) = {
    (resolvers :\ initial) { (r, acc) =>
      acc or Resolvers(r.url)
    }
  }

  /** Creates new POM parser
    */
  def createPomParser(resolver: Resolver) =
    (pkg: MPackage) => resolver.resolveDescriptor(pkg).map(spec =>
      MavenPomParser(spec, (rs => p => chainResolvers(resolver)(rs).resolveDescriptor(p))))

  /** Creates new Meta file parser
    */
  def createMetaParser(resolver: Resolver) =
    (lib: MLib) => resolver.resolveVersions(lib).map(meta => MetaFileParser(meta))
}
