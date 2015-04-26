package bacala.alg

import bacala.core._
import bacala.util.MemoryCache

import org.sat4j.core.{Vec, VecInt}
import org.sat4j.pb._
import org.sat4j.pb.tools.{DependencyHelper, WeightedObject}

import java.math.BigInteger

import scala.collection.JavaConversions._

/** Implements a dependency resolution algorithm based on the SAT4j solver
  */
class SatSolver[T <: Repository](val repository: T) extends Solver {
  type PackageT = repository.PackageT
  type DependencyT = repository.DependencyT
  type ArtifactT = repository.ArtifactT

  abstract class Clause extends Ordered[Clause] {
    def compare(that: Clause) = if (this == that) 0 else this.hashCode() - that.hashCode()
  }

  case class DependencyClause(pkg: PackageT, dep: DependencyT) extends Clause
  case class ConflictClause(artifact: ArtifactT, pkgs: Set[PackageT]) extends Clause

  private val allPackages = repository.packages
  private val allConflicts = repository.conflicts
  // cache the ranking of versions, highest version has lower rank
  private val rankingCache = new MemoryCache[ArtifactT, Map[PackageT, Int]] {}

  // the solver minimizes the objective function, so preferred package has a lower weight
  def weight(pkg: PackageT): Int = {
    def rank(artf: ArtifactT) = {
      allConflicts(artf).toSeq.sortWith { (a, b) =>
        Version(a.version) > Version(b.version)
      }.zip(Stream.from(10)).toMap
    }

    rankingCache.fetch(pkg.artifact, rank(pkg.artifact))(pkg)
  }

  val objectiveFunction: Seq[WeightedObject[PackageT]] = {
    allPackages.map(p => WeightedObject.newWO(p, weight(p))).toSeq
  }

  /** Returns a set of packages if there exists a solution
    */
  override def solve: Either[Set[PackageT], TreeT] = {
    val solver: IPBSolver = SolverFactory.newDefault()
    val helper = new DependencyHelper[PackageT, Clause](solver);

    helper.setObjectiveFunction(objectiveFunction:_*)

    // root constraints
    for ((dep, pkgs) <- repository(repository.root)) {
      helper.clause(DependencyClause(repository.root, dep), pkgs.toSeq:_*)
    }

    // dependencies
    for (p <- allPackages; (dep, pkgs) <- repository(p)) {
      helper.implication(p).implies(pkgs.toSeq:_*).named(DependencyClause(p, dep))
    }

    // conflicts
    for ( (artf, pkgs) <- allConflicts ) {
      helper.atMost(1, pkgs.toSeq:_*).named(ConflictClause(artf, pkgs))
    }

    if (helper.hasASolution) Left(helper.getASolution().toSet)
    else {
      val clauses = helper.why().toList
      Right(constructTree(clauses, repository.root))
    }
  }

  /** Relates two clauses in the unsatisfied clauses to help construct the tree
    */
  def resolveEdge(clauses: Seq[Clause], dep: DependencyT): DependencyEdge[PackageT, DependencyT] = {
    clauses.find {
      case DependencyClause(pkg, _) =>
        dep.artifact == pkg.artifact
      case ConflictClause(artf, pkgs) =>
        dep.artifact == artf
    } map {
      case DependencyClause(pkg, _) =>
        InfectedEdge(dep, pkg)
      case ConflictClause(_, pkgs) =>
        ConflictEdge(dep, pkgs)
    } match {
      case Some(edge) => edge
      case None => MissingEdge(dep)
    }
  }

  /** Constructs the error as a tree for more friendly error report
    */
  def constructTree(clauses: Seq[Clause], node: PackageT): TreeT = {
    val root: TreeT = Node(node)
    (clauses :\ root)  { (clause, tree) =>
      clause match {
        case DependencyClause(pkg, dep) if node == pkg =>
          val edge = resolveEdge(clauses, dep)
          edge match {
            case InfectedEdge(_, to) =>
              tree + (edge, constructTree(clauses, to))
            case _ =>
              tree + (edge, Leaf)
          }
        case _ =>
          tree
      }
    }
  }
}
