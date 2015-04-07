package bacala.maven

import bacala.core._
import bacala.alg.SatSolver

object MavenDependencyManager extends DependencyManager {
  type PackageT = MavenPackage
  var repo: MavenRepository = null

  override def resolve: Option[Iterable[PackageT]] = {
    SatSolver.solve(repo)
  }

  def createRepo(spec: String) = {
    val pom = MavenPomParser(spec, Workers.DefaultPomFetcher)
    repo = new MavenRepository(pom)(Workers.CachedPomFileResolver, Workers.CachedMetaFileResolver)
    repo.construct(Scope.COMPILE)
    println("*****all packages in repository******")
    println(repo.packages.mkString("\n"))
    println("*****all conflicts in repository******")
    println(repo.conflicts.mkString("\n"))
    println("*****resolution result******")
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

      resolve match {
        case Some(set) => println(set.mkString("\n"))
        case None => println("No solution possible")
      }
    }
  }
}
