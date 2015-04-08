package bacala.maven

import bacala.core._
import bacala.util.Worker
import bacala.alg.SatSolver

object MavenDependencyManager extends DependencyManager {
  type PackageT = MavenPackage
  var repo: MavenRepository = null

  override def resolve: Option[Iterable[PackageT]] = {
    SatSolver.solve(repo)
  }

  def createRepo(spec: String) = {
    val pom = MavenPomParser(spec, Workers.DefaultPomFetcher)
    repo = new MavenRepository(pom) {
      override def initPomFetcher = Workers.DefaultPomFetcher
      override def initMetaFetcher = Workers.DefaultMetaFetcher
      override def initPomResolver = Workers.CachedPomFileResolver
      override def initMetaResolver = Workers.CachedMetaFileResolver
      override def makePomFetcher(url: String) = Workers.createPomFetcher(url)
      override def makeMetaFetcher(url: String) = Workers.createMetaFetcher(url)
      override def makePomResolver(fetcher: Worker[MavenPackage, String]) = Workers.createPomResolver(fetcher)
      override def makeMetaResolver(fetcher: Worker[MavenArtifact, String]) = Workers.createMetaResolver(fetcher)
    }
    repo.construct(Scope.COMPILE)
  }

  def printUsage = {
    println(
      """*****************************\n""" +
      """Usage: sbt "run filename.xml"\n""" +
      """*****************************\n"""
    )
  }

  def main(args: Array[String]) = {
    if (args.length != 1) {
      printUsage
    } else {
      val source = io.Source.fromFile(args(0))
      val content = source.mkString
      source.close()

      createRepo(content)

      println("*****resolution result******")
      resolve match {
        case Some(set) => println(set.mkString("\n"))
        case None => println("No solution possible")
      }
    }
  }
}
