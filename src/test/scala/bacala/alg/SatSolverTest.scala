package bacala.test.alg

import bacala.test.BasicSuite
import bacala.alg.SatSolver
import bacala.core._

class SatSolverSuite extends BasicSuite {
  case class Art(val id: String) extends Artifact
  case class Pack(val artifact: Art, val version: String) extends Package
  case class Dep(val artifact: Art, val versionConstraint: String) extends Dependency

  val A = Art("jackson-core")
  val B = Art("jackson-annotations")
  val C = Art("jackson-databind")

  object TestRepository extends Repository {
    type PackageT = Pack

    val map = Map[PackageT, Set[(Dep, Set[PackageT])]](
      Pack(Art("root"), "2.4.2") -> Set(Dep(C, "2.3.4") -> Set(Pack(C, "2.3.4"))),
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
          Pack(B, "2.4.3"),
          Pack(B, "2.4.4")
        )
      ),
      Pack(B, "2.4.2") -> Set(
        Dep(A, "2.4.2") -> Set(Pack(A, "2.4.2"))
      ),
      Pack(B, "2.4.3") -> Set(
        Dep(A, "(2.4.2, 2.4.4)") -> Set(Pack(A, "2.4.3"), Pack(A, "2.4.4"))
      ),
      Pack(B, "2.4.4") -> Set()
    )

    override def root = Pack(Art("root"), "2.4.2")

    override def apply(p: PackageT) = map(p)

    override def packages = map.keys.filter(_ != root)

    override def conflicts = Set(
      A -> Set(
        Pack(Art("jackson-core"), "2.4.2"),
        Pack(Art("jackson-core"), "2.4.3"),
        Pack(Art("jackson-core"), "2.4.4")
      ),
      B -> Set(
        Pack(Art("jackson-annotations"), "2.4.2"),
        Pack(Art("jackson-annotations"), "2.4.3"),
        Pack(Art("jackson-annotations"), "2.4.4")
      )
    )
  }

  test("should be able to solve constraints with the optimal solution") {
    assert(new SatSolver(TestRepository).solve === Left(Set(
      Pack(Art("jackson-annotations"), "2.4.4"),
      Pack(Art("jackson-databind"), "2.3.4"),
      Pack(Art("jackson-core"), "2.4.4")
    )))
  }
}
