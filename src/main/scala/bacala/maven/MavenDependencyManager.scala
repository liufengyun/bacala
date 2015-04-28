package bacala.maven

import bacala.core._
import bacala.util.{Worker, DependencyTree, Measure}
import bacala.alg.SatSolver
import bacala.util.ConsoleHelper.ColorText

object MavenDependencyManager {
  type TreeT = Tree[JPackage, DependencyEdge[JPackage, MavenDependency]]

  def createRepo(spec: String) = {
    val pom = MavenPomParser(spec, Workers.chainPomFetchers(Workers.DefaultPomFetcher))
    val repo = new MavenRepository(pom) with DependencyTree {
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

    repo
  }

  def printUsage = {
    println(
      """=============================\n""" +
      """Usage: sbt "run filename.xml"\n""" +
      """=============================\n"""
    )
  }

  def printTree(tree: TreeT, level: Int = 0): Unit = {
    tree match {
      case Node(pkg, children) =>
        if (level != 0) println("  " * (level - 1) + pkg)
        children.foreach { case (edge, child) =>
          edge match {
            case InfectedEdge(dep, to) =>
              printTree(child, level+1) // depth-first
            case ConflictEdge(dep, conflicts) =>
              println(("  " * level + "Dependency " + dep + " leads to conflict").red)
            case MissingEdge(dep) =>
              println(("  " * level + "Dependency " + dep + " can't be resolved").red)
            case HealthyEdge(dep, to) =>
              printTree(child, level+1) // depth-first
          }
        }
      case Leaf =>
    }
  }

  def run(file: String) = {
    val source = io.Source.fromFile(file)
    val content = source.mkString
    source.close()

    val measure = new Measure()
    val repo = measure.time("Network IO") { createRepo(content) }
    val solver = new SatSolver(repo)

    println("\n\n================  Resolution Result   ==================".bold)
    measure.time("Resolution") { solver.solve } match {
      case Left(set) => printTree(repo.buildTree(set))
      case Right(tree) => printTree(tree)
    }

    println(measure)
  }

  def main(args: Array[String]) = {
    if (args.length != 1)
      printUsage
    else
      run(args(0))
  }
}
