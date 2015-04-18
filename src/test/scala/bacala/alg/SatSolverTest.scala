package bacala.test.alg

import bacala.test.BasicSuite
import bacala.alg.SatSolver
import bacala.core._

class SatSolverSuite extends BasicSuite {
  case class Art(val id: String) extends Artifact
  case class Pack(val artifact: Art, val version: String) extends Package

  object TestRepository extends Repository {
    type PackageT = Pack

    val map = Map[PackageT, Set[Set[PackageT]]](
      Pack(Art("root"), "2.4.2") -> Set(Set(Pack(Art("jackson-databind"), "2.3.4"))),
      Pack(Art("jackson-core"), "2.4.2") -> Set(),
      Pack(Art("jackson-core"), "2.4.3") -> Set(),
      Pack(Art("jackson-databind"), "2.3.4") -> Set(
        Set(
          Pack(Art("jackson-core"), "2.4.2"),
          Pack(Art("jackson-core"), "2.4.3")
        ),
        Set(
          Pack(Art("jackson-annotations"), "2.4.2"),
          Pack(Art("jackson-annotations"), "2.4.3")
        )
      ),
      Pack(Art("jackson-annotations"), "2.4.2") -> Set(
        Set(Pack(Art("jackson-core"), "2.4.2"))
      ),
      Pack(Art("jackson-annotations"), "2.4.3") -> Set(
        Set(Pack(Art("jackson-core"), "2.4.3"))
      )
    )

    override def root = Pack(Art("root"), "2.4.2")

    override def apply(p: PackageT) = map(p)

    override def packages = map.keys

    override def conflicts = Set(
      Seq(Pack(Art("jackson-core"), "2.4.2"), Pack(Art("jackson-core"), "2.4.3")),
      Seq(Pack(Art("jackson-annotations"), "2.4.2"), Pack(Art("jackson-annotations"), "2.4.3"))
    )
  }

  test("should be able to solve constraints with the optimal solution") {
    assert(new SatSolver(TestRepository).solve === Some(Set(
      Pack(Art("jackson-annotations"), "2.4.3"),
      Pack(Art("jackson-databind"), "2.3.4"),
      Pack(Art("jackson-core"), "2.4.3")
    )))
  }
}
