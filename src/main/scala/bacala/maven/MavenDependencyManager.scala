package bacala.maven

import bacala._

object MavenDependencyManager extends DependencyManager {
  type Result = Set[MavenPackage]
  type ConstraintsT = Set[Set[MavenPackage]]

  override def resolve(initial: ConstraintsT): Result = {
    val repo = new MavenRepository()
    repo.initialize(initial, MavenPomParser(_))
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

      val result = resolve(MavenPomParser(content, Scope.COMPILE))
    }
  }
}
