package bacala.maven

import bacala.core._
import bacala.util.Cache

object PomFileCachedFetcher extends (MavenPackage => Option[String]) with Cache[MavenPackage, Option[String]] {
  override def apply(pkg: MavenPackage) = fetch(pkg, MavenFetcher(pkg))
}

object PomFileParser extends MavenPomParser {
  val fetcher = PomFileCachedFetcher
}

object MavenDependencyManager extends DependencyManager {
  type PackageT = MavenPackage
  type DependencyT = MavenDependency

  object Parser extends (MavenPackage => Option[Iterable[MavenDependency]]) with Cache[MavenPackage, Option[Iterable[MavenDependency]]] {
    override def apply(pkg: MavenPackage) = fetch(pkg, MavenFetcher(pkg).map(spec => PomFileParser(spec)))
  }

  override def resolve(initial: Iterable[DependencyT]): Iterable[PackageT] = {
    val repo = new MavenRepository(initial, Parser)
    repo.construct(Scope.COMPILE)
    println("*****all packages in repository******")
    println(repo.packages.mkString("\n"))
    println("*****all conflicts in repository******")
    println(repo.conflicts.mkString("\n"))
    Set()
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

      val result = resolve(PomFileParser(content))
    }
  }
}
