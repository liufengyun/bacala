package bacala.alg

import bacala.core._
import org.sat4j.core.{Vec, VecInt}
import org.sat4j.pb._
import org.sat4j.pb.tools.{DependencyHelper, WeightedObject}
import java.math.BigInteger
import scala.collection.JavaConversions._

/** Implements a dependency resolution algorithm based on the SAT4j solver
  */
class SatSolver[T <: Repository](val repository: T) extends Solver {

  abstract class Clause extends Ordered[Clause] {
    def compare(that: Clause) = if (this == that) 0 else this.hashCode() - that.hashCode()
  }

  case class DependencyClause(pkg: PackageT, dep: DependencyT) extends Clause
  case class ConflictClause(artifact: Artifact, pkgs: Set[PackageT]) extends Clause

  type PackageT = repository.PackageT
  type DependencyT = repository.DependencyT
  // data
  val packages = repository.packages
  val conflicts = repository.conflicts

  // the solver minimize objective function, so preferred package has a lower weight
  def weight(pkg: PackageT): Int = {
    val Version(major, minor, revision, qualifier, build) = Version(pkg.version)
    major * -100 - minor * 10 - revision
    // 0x7FFFFFFF / (major * 10000 + minor * 100 + 10000*revision)
  }

  val objectiveFunction: Seq[WeightedObject[PackageT]] = {
    packages.map(p => WeightedObject.newWO(p, weight(p))).toSeq
  }

  /** Returns a set of packages if there exists a solution
    */
  override def solve: Either[Set[PackageT], TreeT] = {
    val solver: IPBSolver = SolverFactory.newEclipseP2()
    val helper = new DependencyHelper[PackageT, Clause](solver);

    helper.setObjectiveFunction(objectiveFunction:_*)

    // root constraints
    for ((dep, pkgs) <- repository(repository.root)) {
      helper.clause(DependencyClause(repository.root, dep), pkgs.toSeq:_*)
    }

    // dependencies
    for (p <- packages; (dep, pkgs) <- repository(p)) {
      helper.implication(p).implies(pkgs.toSeq:_*).named(DependencyClause(p, dep))
    }

    // conflicts
    for ( (artf, pkgs) <- conflicts ) {
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
