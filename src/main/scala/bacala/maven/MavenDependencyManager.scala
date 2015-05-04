package bacala.maven

import bacala.core._
import bacala.util._
import bacala.alg.SatSolver
import bacala.util.ConsoleHelper.ColorText

object MavenDependencyManager {
  type TreeT = Tree[MPackage, DependencyEdge[MPackage, MDependency]]

  def createRepo(spec: String) = {
    val metaFileResolverCache = new CachedWorker[MLib, Iterable[String]] with
        MemoryBase[MLib, Iterable[String]]

    val pomFileResolverCache = new CachedWorker[MPackage, MDescriptor] with
        MemoryBase[MPackage, MDescriptor]

    val pom = MavenPomParser(spec, rs => p =>
      Workers.chainResolvers(Workers.DefaultResolver)(rs).resolveDescriptor(p))

    val repo = new MavenRepository(pom) with DependencyTree {
      override def makeResolver(resolvers: Iterable[MResolver]) = {
        val resolver = Workers.chainResolvers(Workers.DefaultResolver)(resolvers)

        (pomFileResolverCache or Workers.createPomParser(resolver),
          metaFileResolverCache or Workers.createMetaParser(resolver))
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
