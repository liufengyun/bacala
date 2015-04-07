package bacala.test

import org.scalatest._
import bacala.maven._

class MavenDependencySuite extends BasicSuite {
  test("compatible set should include the specified version") {
    val artf = MavenArtifact("group", "id")
    val dep = MavenDependency(artf, "1.0", List(), Scope.COMPILE, true)

    val versions = List("0.2", "0.5", "1.3")

    assert(dep.resolve(versions) == Set(
      MavenPackage(artf, "1.0"),
      MavenPackage(artf, "1.3")
    ))
  }
}
