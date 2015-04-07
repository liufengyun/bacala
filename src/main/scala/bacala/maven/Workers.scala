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

  /** Creates new POM file fetcher
    */
  def createPomFetcher(url: String) = new PomFetcher(url)

  /** Creates new Meta file fetcher
    */
  def createMetaFetcher(url: String) = new MetaFetcher(url)

  /** Creates new POM file resolver
    */
  def createPomResolver(url: String) = new Worker[MavenPackage, MavenPomFile] {
    val fetcher = createPomFetcher(url)
    override def apply(pkg: MavenPackage) = {
      fetcher(pkg).map(spec => MavenPomParser(spec, fetcher))
    }
  }

  /** Creates new Meta file resolver
    */
  def createMetaResolver(url: String) = new Worker[MavenArtifact, Iterable[String]] {
    val fetcher = createMetaFetcher(url)
    override def apply(artf: MavenArtifact) = {
      fetcher(artf).map(meta => MetaFileParser(meta))
    }
  }
}
