package bacala.ivy

import bacala.alg.SatSolver
import bacala.core._
import bacala.util.DependencyTree
import bacala.util.Measure
import bacala.util.ConsoleHelper.ColorText
import java.nio.file.Paths
import scala.io.Source

object IvyDependencyManager {
  type TreeT = Tree[IPackage, DependencyEdge[IPackage, IDependency]]

  def run(setting: String, content: String, config: String) = {
    val absoluteSetting = if (setting == "") "" else Paths.get(setting).toAbsolutePath.toString
    val parser = new IvyParser(absoluteSetting)
    val initial = parser.parse(content)
    val configuration = if (config == "") "default" else config

    val ivyCache = new IvyCache(".bacala_cache/ivy/", parser.parse)
    val versionCache = new VersionsCache(".bacala_cache/ivy/")

    implicit val repo = new IvyRepository(initial) with DependencyTree {
      def versionsResolver(lib: LibT) = {
        if (versionCache.exists(lib))
          Some(versionCache.fetch(lib))
        else
          parser.listRevisions(lib) match {
            case Some(l) =>
              versionCache.update(lib, l)
              Some(l)
            case None => None
          }
      }

      def descriptorResolver(pkg: PackageT) = {
        if (ivyCache.exists(pkg))
          Some(ivyCache.fetch(pkg))
        else
          parser.getDescriptor(pkg) match {
            case Some(d) =>
              ivyCache.update(pkg, d)
              Some(d)
            case None => None
          }
      }
    }

    val measure = new Measure()
    measure.time("Network IO") { repo.construct(configuration) }
    repo.dump


    val solver = new SatSolver(repo)

    println("\n\n================  Resolution Result   ==================".bold)
    measure.time("Resolution") { solver.solve } match {
      case Left(set) =>
        printTree(repo.buildTree(set))
      case Right(tree) => printTree(tree)
    }

    println(measure)
  }

  def printTree(tree: TreeT, level: Int = 0)(implicit repo: IvyRepository): Unit = {
    tree match {
      case Node(pkg, children) =>

        if (level != 0) {
          val artifacts = repo.artifacts(pkg)
          if (artifacts.size == 0 || (artifacts.size == 1 && artifacts.head == pkg.lib.name)) {
            println("  " * (level - 1) + pkg)
          } else {
            println("  " * (level - 1) + pkg + "(" + artifacts.mkString(",") + ")")
          }
        }

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

  def readFile(path: String): String = {
    val longPath = Paths.get(path).toAbsolutePath.toString
    Source.fromFile(longPath).mkString
  }


  def main(args: Array[String]) = {
    if (args.length == 1)
      run("", readFile(args(0)), "")
    else if (args.length == 2)
      run("", readFile(args(0)), args(1))
    else if (args.length == 3)
      run(args(0), readFile(args(1)), args(2))
    else
      println("Usage: run [<setting xml>] <ivy xml> [config]")
  }
}
