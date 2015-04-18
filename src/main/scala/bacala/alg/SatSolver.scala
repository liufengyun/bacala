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
  type PackageT = repository.PackageT
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
  override def solve: Either[Set[PackageT], Seq[Clause]] = {
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
    else Right(helper.why().toList)
  }
}
