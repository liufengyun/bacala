package bacala.alg

import bacala.core._
import org.sat4j.core.{Vec, VecInt}
import org.sat4j.pb._
import java.math.BigInteger

/** Implements a dependency resolution algorithm based on the SAT4j solver
  */
class SatSolver[T <: Repository](val repository: T) extends Solver {
  type PackageT = repository.PackageT
  // data
  // important for packages to be ordered for package2Int and int2Package to work
  val packages = repository.packages.toList
  val conflicts = repository.conflicts
  val package2Int = packages.zip(Stream from 1).toMap
  val int2Package = (Stream from 1).zip(packages).toMap

  // helper methods
  def clauseSize = conflicts.size +
    (packages :\ 0) {(p, acc) => repository(p).size + acc }

  def clauseForPackage(p: PackageT): Iterable[VecInt] = {
    repository(p).map { set =>
      if (repository.root == p)
        new VecInt(set.map(package2Int(_)).toArray)
      else
        new VecInt(-package2Int(p) +: set.map(package2Int(_)).toArray)
    }
  }

  def addConflict(conflict: Iterable[PackageT], solver: IPBSolver): Unit = {
    val literals = new VecInt(conflict.map(package2Int(_)).toArray)
    val coeffs = new VecInt(conflict.map(_ => 1).toArray)
    solver.addAtMost(literals, coeffs, 1)
  }

  def weight(pkg: PackageT): Int = {
    val Version(major, minor, revision, qualifier, build) = Version(pkg.version)
    major * -100 - minor * 10 - revision
  }

  val objectiveFunction: ObjectiveFunction = {
    val literals = new VecInt(packages.map(package2Int(_)).toArray)
    val coeffs = new Vec(packages.map(pkg => BigInteger.valueOf(weight(pkg))).toArray)
    new ObjectiveFunction(literals, coeffs)
  }

  /** Returns a set of packages if there exists a solution
    */
  override def solve: Option[Iterable[PackageT]] = {

    val solver: IPBSolver = SolverFactory.newDefault()

    // number of vars
    solver.newVar(packages.size)
    solver.setExpectedNumberOfClauses(clauseSize)

    for {
      p <- packages
      clause <- clauseForPackage(p)
    } solver.addClause(clause)

    conflicts.foreach { c => addConflict(c, solver) }

    solver.setObjectiveFunction(objectiveFunction)

    val res = solver.findModel()
    if (res == null) None else
      Some(res.filter(_ > 0).map(int2Package(_)).toSet)
  }
}
