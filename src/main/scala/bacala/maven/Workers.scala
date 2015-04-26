package bacala.maven

import bacala.core._
import bacala.util._

/** Workers factory
  */
object Workers {
  object MetaFileResolverCache extends CachedWorker[MavenArtifact, Iterable[String]] with MemoryBase[MavenArtifact, Iterable[String]]

  object PomFileResolverCache extends CachedWorker[MavenPackage, MavenPomData] with MemoryBase[MavenPackage, MavenPomData]

  val mavenMainBase = "http://repo1.maven.org/maven2"

  /** Fetches POM file from the Maven main repository
    */
  val DefaultPomFetcher = PomFetchers(mavenMainBase)

  /** Fetches Meta file from the Maven main repository
    */
  val DefaultMetaFetcher = MetaFetchers(mavenMainBase)

  /** Cache and reuses the POM fetchers
    */
  object PomFetchers extends MemoryCache[String, Worker[MavenPackage, String]] {
    def apply(url: String) = fetch(url, new CachedWorker[MavenPackage, String] with
      MemoryBase[MavenPackage, String] {
      override val worker = new PomFetcher(url)
    })
  }

  /** Cache and reuses the Meta fetchers
    */
  object MetaFetchers extends MemoryCache[String, Worker[MavenArtifact, String]] {
    def apply(url: String) = fetch(url, new CachedWorker[MavenArtifact, String] with
      MemoryBase[MavenArtifact, String] {
      override val worker = new MetaFetcher(url)
    })
  }

  /** Creates new POM file fetcher
    */
  def createPomFetcher(url: String) = PomFetchers(url)

  /** Creates new Meta file fetcher
    */
  def createMetaFetcher(url: String) = MetaFetchers(url)

  /** Creates a chain of POM fetchers
    */
  def chainPomFetchers(initial: Worker[MavenPackage, String])(resolvers: Iterable[MavenResolver]) = {
    (resolvers :\ initial) { (r, acc) =>
      acc or Workers.createPomFetcher(r.url)
    }
  }

  /** Creates a chain of Meta fetchers
    */
  def chainMetaFetchers(initial: Worker[MavenArtifact, String])(resolvers: Iterable[MavenResolver]) = {
    (resolvers :\ initial) { (r, acc) =>
      acc or Workers.createMetaFetcher(r.url)
    }
  }

  /** Creates new POM file resolver
    *
    * Problem: now for the POM parser, it can only use a single fetcher, unable to chain them
    * but the parent or modules of a POM file may be found at different repos.
    */
  def createPomResolver(fetcher: Worker[MavenPackage, String]) = new Worker[MavenPackage, MavenPomData] {
    override def apply(pkg: MavenPackage) = {
      fetcher(pkg).map(spec => MavenPomParser(spec, chainPomFetchers(fetcher)))
    }
  }

  /** Creates new Meta file resolver
    */
  def createMetaResolver(fetcher: Worker[MavenArtifact, String]) = new Worker[MavenArtifact, Iterable[String]] {
    override def apply(artf: MavenArtifact) = {
      fetcher(artf).map(meta => MetaFileParser(meta))
    }
  }
}
