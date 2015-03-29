package bacala.maven

import bacala.core._

object MavenDependencyManager extends DependencyManager {
  type PackageT = MavenPackage
  type DependencyT = MavenDependency

  override def resolve(initial: Iterable[DependencyT]): Iterable[PackageT] = {
    val repo = new MavenRepository(initial)
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

      val result = resolve(MavenPomParser(content))
    }
  }
}
