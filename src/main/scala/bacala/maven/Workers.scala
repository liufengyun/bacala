package bacala.maven

import bacala.core._
import bacala.util._

/** Workers factory
  */
object Workers {
  object MetaFileResolverCache extends CachedWorker[MLib, Iterable[String]] with MemoryBase[MLib, Iterable[String]]

  object PomFileResolverCache extends CachedWorker[MPackage, MFile] with MemoryBase[MPackage, MFile]

  val mavenMainBase = "http://repo1.maven.org/maven2"

  /** Fetches POM file from the Maven main repository
    */
  val DefaultPomFetcher = PomFetchers(mavenMainBase)

  /** Fetches Meta file from the Maven main repository
    */
  val DefaultMetaFetcher = MetaFetchers(mavenMainBase)

  /** Cache and reuses the POM fetchers
    */
  object PomFetchers extends MemoryCache[String, Worker[MPackage, String]] {
    def apply(url: String) = fetch(url, new CachedWorker[MPackage, String] with
      MemoryBase[MPackage, String] {
      override val worker = new PomFetcher(url)
    })
  }

  /** Cache and reuses the Meta fetchers
    */
  object MetaFetchers extends MemoryCache[String, Worker[MLib, String]] {
    def apply(url: String) = fetch(url, new CachedWorker[MLib, String] with
      MemoryBase[MLib, String] {
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
  def chainPomFetchers(initial: Worker[MPackage, String])(resolvers: Iterable[MResolver]) = {
    (resolvers :\ initial) { (r, acc) =>
      acc or Workers.createPomFetcher(r.url)
    }
  }

  /** Creates a chain of Meta fetchers
    */
  def chainMetaFetchers(initial: Worker[MLib, String])(resolvers: Iterable[MResolver]) = {
    (resolvers :\ initial) { (r, acc) =>
      acc or Workers.createMetaFetcher(r.url)
    }
  }

  /** Creates new POM file resolver
    */
  def createPomResolver(fetcher: Worker[MPackage, String]) = new Worker[MPackage, MFile] {
    override def apply(pkg: MPackage) = {
      fetcher(pkg).map(spec => MavenPomParser(spec, chainPomFetchers(fetcher)))
    }
  }

  /** Creates new Meta file resolver
    */
  def createMetaResolver(fetcher: Worker[MLib, String]) = new Worker[MLib, Iterable[String]] {
    override def apply(lib: MLib) = {
      fetcher(lib).map(meta => MetaFileParser(meta))
    }
  }
}
