package bacala.alg

import bacala.core._
import org.sat4j.specs._
import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory

/** Implements a dependency resolution algorithm based on the SAT4j solver
  */
object SatSolver extends Solver {
  /** Returns a set of packages if there exists a solution
    */
  override def solve[T <: Repository](repository: T): Option[Iterable[repository.PackageT]] = {
    type PackageT = repository.PackageT
    // data
    val packages = repository.packages
    val conflicts = repository.conflicts
    val package2Int = packages.zip(Stream from 1).toMap
    val int2Package = (Stream from 1).zip(packages).toMap

    // helper methods
    def clauseSize = (packages :\ 0) {(p, acc) => repository(p).size + acc } + conflicts.size
    def initialClause = {
      repository.initial.map { set =>
        new VecInt(set.map(package2Int(_)).toArray)
      }
    }

    def clauseForPackage(p: PackageT): Iterable[VecInt] = {
      repository(p).filter(_.size != 0).map { set =>
        new VecInt(-package2Int(p) +: set.map(package2Int(_)).toArray)
      }
    }

    def clauseForConflict(conflict: (PackageT, PackageT)): VecInt = {
      new VecInt(Array(-package2Int(conflict._1), -package2Int(conflict._2)))
    }

    val solver: ISolver = SolverFactory.newDefault()

    // number of vars
    solver.newVar(packages.size)
    solver.setExpectedNumberOfClauses(clauseSize)

    initialClause.foreach { clause => solver.addClause(clause) }

    for {
      p <- packages
      clause <- clauseForPackage(p)
    } solver.addClause(clause)

    conflicts.foreach { c => solver.addClause(clauseForConflict(c)) }

    val res = solver.findModel()
    if (res == null) None else
      Some(res.filter(_ > 0).map(int2Package(_)).toSet)
  }
}
