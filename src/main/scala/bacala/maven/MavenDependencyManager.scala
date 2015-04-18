package bacala.maven

import bacala.core._
import bacala.util.Worker
import bacala.alg.SatSolver

object MavenDependencyManager extends DependencyManager {
  type PackageT = MavenPackage
  var repo: MavenRepository = null

  override def resolve: Either[Iterable[PackageT], Seq[Clause]] = {
    new SatSolver(repo).solve
  }

  def createRepo(spec: String) = {
    val pom = MavenPomParser(spec, Workers.chainPomFetchers(Workers.DefaultPomFetcher))
    repo = new MavenRepository(pom) {
      override def makePomResolver(resolvers: Iterable[MavenResolver]) = {
        val fetcher = Workers.chainPomFetchers(Workers.DefaultPomFetcher)(resolvers)
        Workers.PomFileResolverCache or Workers.createPomResolver(fetcher)
      }

      override def makeMetaResolver(resolvers: Iterable[MavenResolver]) = {
        val fetcher = Workers.chainMetaFetchers(Workers.DefaultMetaFetcher)(resolvers)
        Workers.MetaFileResolverCache or Workers.createMetaResolver(fetcher)
      }
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

      println("\n\n######## resolution result #########")
      resolve match {
        case Left(set) => println(set.mkString("\n"))
        case Right(seq) => println(seq.mkString("\n"))
      }
    }
  }
}
