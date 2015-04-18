package bacala.test.alg

import bacala.test.BasicSuite
import bacala.alg.SatSolver
import bacala.core._

class SatSolverSuite extends BasicSuite {
  object TestRepository extends MiniRepository {
    val map = Map[PackageT, Set[(Dep, Set[PackageT])]](
      Pack(Root, "2.4.2") -> Set(Dep(C, "2.3.4") -> Set(Pack(C, "2.3.4"))),
      Pack(A, "2.4.2") -> Set(),
      Pack(A, "2.4.3") -> Set(),
      Pack(A, "2.4.4") -> Set(),
      Pack(C, "2.3.4") -> Set(
        Dep(A, "[2.4.2, 2.4.4]") -> Set(
          Pack(A, "2.4.2"),
          Pack(A, "2.4.3"),
          Pack(A, "2.4.4")
        ),
        Dep(B, "[2.4.2, 2.4.4]") -> Set(
          Pack(B, "2.4.2"),
          Pack(B, "2.4.3")
        )
      ),
      Pack(B, "2.4.2") -> Set(
        Dep(A, "2.4.2") -> Set(Pack(A, "2.4.2"))
      ),
      Pack(B, "2.4.3") -> Set(
        Dep(A, "(2.4.2, 2.4.4)") -> Set(Pack(A, "2.4.3"), Pack(A, "2.4.4"))
      )
    )

    override def conflicts = Set(
      A -> Set(
        Pack(A, "2.4.2"),
        Pack(A, "2.4.3"),
        Pack(A, "2.4.4")
      ),
      B -> Set(
        Pack(B, "2.4.2"),
        Pack(B, "2.4.3")
      )
    )
  }

  test("should be able to solve constraints with the optimal solution") {
    assert(new SatSolver(TestRepository).solve === Left(Set(
      Pack(B, "2.4.3"),
      Pack(C, "2.3.4"),
      Pack(A, "2.4.4")
    )))
  }

  object ConflictRepository extends MiniRepository {
    val map = Map[PackageT, Set[(Dep, Set[PackageT])]](
      Pack(Root, "2.4.2") -> Set(
        Dep(A, "1.3.5") -> Set(Pack(A, "1.3.5")),
        Dep(B, "2.3.4") -> Set(Pack(B, "2.3.4"))
      ),
      Pack(A, "1.3.5") -> Set(
        Dep(C, "2.1.1") -> Set(Pack(C, "2.1.1"))
      ),
      Pack(B, "2.3.4") -> Set(
        Dep(C, "2.1.4") -> Set(Pack(C, "2.1.4"))
      ),
      Pack(C, "2.1.1") -> Set(),
      Pack(C, "2.1.4") -> Set()
    )

    override def conflicts = Set(
      A -> Set(Pack(A, "1.3.5")),
      B -> Set(Pack(B, "2.3.4")),
      C -> Set(Pack(C, "2.1.1"), Pack(C, "2.1.4"))
    )
  }

  test("report error if there's no solution") {
    assert(new SatSolver(ConflictRepository).solve === Right(List(
      DependencyClause(Pack(A, "1.3.5"), Dep(C, "2.1.1")),
      ConflictClause(C, Set(Pack(C, "2.1.1"), Pack(C, "2.1.4"))),
      DependencyClause(Pack(B, "2.3.4"), Dep(C, "2.1.4")),
      DependencyClause(Pack(Root, "2.4.2"), Dep(A, "1.3.5")),
      DependencyClause(Pack(Root, "2.4.2"), Dep(B, "2.3.4"))
    )))
  }

  object IncompleteRepository extends MiniRepository {
    val map = Map[PackageT, Set[(Dep, Set[PackageT])]](
      Pack(Root, "2.4.2") -> Set(
        Dep(A, "1.3.5") -> Set(Pack(A, "1.3.5")),
        Dep(B, "2.3.4") -> Set(Pack(B, "2.3.4"))
      ),
      Pack(A, "1.3.5") -> Set(
        Dep(D, "1.1.1") -> Set()
      ),
      Pack(B, "2.3.4") -> Set(
        Dep(C, "2.1.4") -> Set(Pack(C, "2.1.4"))
      ),
      Pack(C, "2.1.4") -> Set()
    )

    override def conflicts = Set(
      A -> Set(Pack(A, "1.3.5")),
      B -> Set(Pack(B, "2.3.4")),
      C -> Set(Pack(C, "2.1.1"), Pack(C, "2.1.4"))
    )
  }

  test("report error when there's missing dependency") {
    assert(new SatSolver(IncompleteRepository).solve === Right(List(
      DependencyClause(Pack(A, "1.3.5"), Dep(D, "1.1.1")),
      DependencyClause(Pack(Root, "2.4.2"), Dep(A, "1.3.5"))
    )))
  }
}
