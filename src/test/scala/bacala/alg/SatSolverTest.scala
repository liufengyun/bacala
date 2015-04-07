package bacala.test.alg

import bacala.test.BasicSuite
import bacala.alg.SatSolver
import bacala.core._
import bacala.maven._

class SatSolverSuite extends BasicSuite {
  object TestRepository extends Repository {
    type PackageT = MavenPackage

    val map = Map[PackageT, Set[Set[PackageT]]](
      MavenPackage(MavenArtifact("root", "root"), "2.4.2") -> Set(Set(MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-databind"), "2.3.4"))),
      MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-core"), "2.4.2") -> Set(),
      MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-core"), "2.4.3") -> Set(),
      MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-databind"), "2.3.4") -> Set(
        Set(
          MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-core"), "2.4.2"),
          MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-core"), "2.4.3")
        ),
        Set(
          MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-annotations"), "2.4.2"),
          MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-annotations"), "2.4.3")
        )
      ),
      MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-annotations"), "2.4.2") -> Set(
        Set(MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-core"), "2.4.2"))
      ),
      MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-annotations"), "2.4.3") -> Set(
        Set(MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-core"), "2.4.3"))
      )
    )

    override def root = MavenPackage(MavenArtifact("root", "root"), "2.4.2")

    override def apply(p: PackageT) = map(p)

    override def packages = map.keys

    override def conflicts = Set(
      MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-core"), "2.4.2") ->
        MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-core"), "2.4.3"),
      MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-annotations"), "2.4.2") ->
        MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-annotations"), "2.4.3")
    )
  }

  test("should be able to solve simple constraints") {
    assert(SatSolver.solve(TestRepository) === Some(Set(
      MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-annotations"), "2.4.3"),
      MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-databind"), "2.3.4"),
      MavenPackage(MavenArtifact("com.fasterxml.jackson.core", "jackson-core"), "2.4.3")
    )))
  }
}
