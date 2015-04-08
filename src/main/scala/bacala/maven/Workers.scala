package bacala.maven

import bacala.core._
import bacala.util._

/** Workers factory
  */
object Workers {
  object CachedMetaFileResolver extends CachedWorker[MavenArtifact, Iterable[String]] {
    override val worker = new Worker[MavenArtifact, Iterable[String]] {
      override def apply(artf: MavenArtifact) = {
        DefaultMetaFetcher(artf).map(meta => MetaFileParser(meta))
      }
    }

    override val cache = new MemoryCache[MavenArtifact, Option[Iterable[String]]] {}
  }

  object CachedPomFileResolver extends CachedWorker[MavenPackage, MavenPomFile] {
    override val worker = new Worker[MavenPackage, MavenPomFile] {
      override def apply(pkg: MavenPackage) = {
        DefaultPomFetcher(pkg).map(spec => MavenPomParser(spec, DefaultPomFetcher))
      }
    }

    override val cache = new MemoryCache[MavenPackage, Option[MavenPomFile]] {}
  }

  val mavenMainBase = "http://repo1.maven.org/maven2"

  /** Fetches POM file from the Maven main repository
    */
  object DefaultPomFetcher extends CachedWorker[MavenPackage, String] {
    override val worker = new PomFetcher(mavenMainBase)
    override val cache = new MemoryCache[MavenPackage, Option[String]] {}
  }

  /** Fetches Meta file from the Maven main repository
    */
  object DefaultMetaFetcher extends CachedWorker[MavenArtifact, String] {
    override val worker = new MetaFetcher(mavenMainBase)
    override val cache = new MemoryCache[MavenArtifact, Option[String]] {}
  }

  /** Cache and reuses the POM fetchers
    */
  object PomFetchers extends MemoryCache[String, Worker[MavenPackage, String]] {
    def apply(url: String) = fetch(url, new CachedWorker[MavenPackage, String] {
      override val worker = new PomFetcher(url)
      override val cache = new MemoryCache[MavenPackage, Option[String]] {}
    })
  }

  /** Cache and reuses the Meta fetchers
    */
  object MetaFetchers extends MemoryCache[String, Worker[MavenArtifact, String]] {
    def apply(url: String) = fetch(url, new CachedWorker[MavenArtifact, String] {
      override val worker = new MetaFetcher(url)
      override val cache = new MemoryCache[MavenArtifact, Option[String]] {}
    })
  }

  /** Creates new POM file fetcher
    */
  def createPomFetcher(url: String) = PomFetchers(url)

  /** Creates new Meta file fetcher
    */
  def createMetaFetcher(url: String) = MetaFetchers(url)

  /** Creates new POM file resolver
    *
    * Problem: now for the POM parser, it can only use a single fetcher, unable to chain them
    * but the parent or modules of a POM file may be found at different repos.
    */
  def createPomResolver(fetcher: Worker[MavenPackage, String]) = new Worker[MavenPackage, MavenPomFile] {
    override def apply(pkg: MavenPackage) = {
      fetcher(pkg).map(spec => MavenPomParser(spec, fetcher))
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
