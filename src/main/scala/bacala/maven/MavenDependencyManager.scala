package bacala.maven

import bacala.core._
import bacala.util.Cache
import bacala.alg.SatSolver

object PomFileCachedFetcher extends (MavenPackage => Option[String]) with Cache[MavenPackage, Option[String]] {
  override def apply(pkg: MavenPackage) = fetch(pkg, MavenFetcher(pkg))
}

object PomFileParser extends MavenPomParser {
  val fetcher = PomFileCachedFetcher
}

object CachedMetaFileParser extends (MavenArtifact => Option[Iterable[String]]) with Cache[MavenArtifact, Option[Iterable[String]]] {
  override def apply(artf: MavenArtifact) = fetch(artf, MavenFetcher.getMetaData(artf).map(meta => MetaFileParser(meta)))
}

object MavenDependencyManager extends DependencyManager {
  type PackageT = MavenPackage
  type DependencyT = MavenDependency

  object Parser extends (MavenPackage => Option[Iterable[MavenDependency]]) with Cache[MavenPackage, Option[Iterable[MavenDependency]]] {
    override def apply(pkg: MavenPackage) = fetch(pkg, MavenFetcher(pkg).map(spec => PomFileParser(spec)))
  }

  override def resolve(initial: Iterable[DependencyT]): Option[Iterable[PackageT]] = {
    val repo = new MavenRepository(initial)(Parser, CachedMetaFileParser)
    repo.construct(Scope.COMPILE)
    println("*****all packages in repository******")
    println(repo.packages.mkString("\n"))
    println("*****all conflicts in repository******")
    println(repo.conflicts.mkString("\n"))
    println("*****resolution result******")

    SatSolver.solve(repo)
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

      resolve(PomFileParser(content)) match {
        case Some(set) => println(set.mkString("\n"))
        case None => println("No solution possible")
      }
    }
  }
}
