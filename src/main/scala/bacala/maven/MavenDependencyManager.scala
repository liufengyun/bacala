package bacala.maven

import bacala.core._
import bacala.util.{Worker, DependencyTree, Measure}
import bacala.alg.SatSolver
import bacala.util.ConsoleHelper.ColorText

object MavenDependencyManager {
  type PackageT = MavenPackage
  type TreeT = Tree[PackageT, DependencyEdge[PackageT, MavenDependency]]

  var repo: MavenRepository with DependencyTree = null

  def resolve: Either[Set[PackageT], TreeT] = {
    new SatSolver(repo).solve
  }

  def createRepo(spec: String) = {
    val pom = MavenPomParser(spec, Workers.chainPomFetchers(Workers.DefaultPomFetcher))
    repo = new MavenRepository(pom) with DependencyTree {
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
      """=============================\n""" +
      """Usage: sbt "run filename.xml"\n""" +
      """=============================\n"""
    )
  }

  def printTree(tree: TreeT, level: Int = 0): Unit = {
    def tip = "  " * (level - 1)
    tree match {
      case Node(pkg, children) =>
        if (level != 0) println(tip + pkg)
        children.foreach { case (edge, child) =>
          edge match {
            case InfectedEdge(dep, to) =>
              printTree(child, level+1) // depth-first
            case ConflictEdge(dep, conflicts) =>
              println((tip + "Dependency " + dep + " leads to conflict").red)
            case MissingEdge(dep) =>
              println((tip + "Dependency " + dep + " can't be resolved").red)
            case HealthyEdge(dep, to) =>
              printTree(child, level+1) // depth-first
          }
        }
      case Leaf =>
    }
  }

  def main(args: Array[String]) = {
    if (args.length != 1) {
      printUsage
    } else {
      val source = io.Source.fromFile(args(0))
      val content = source.mkString
      source.close()

      val measure = new Measure()

      measure.time("Network IO") { createRepo(content) }

      println("\n\n================  Resolution Result   ==================".bold)
      measure.time("Resolution") { resolve } match {
        case Left(set) => printTree(repo.buildTree(set))
        case Right(tree) => printTree(tree)
      }

      println(measure)
    }
  }
}
