package bacala.test.maven

import bacala.test._
import bacala.maven._

class MavenDependencySuite extends BasicSuite {
  test("compatible set should include the specified version") {
    val lib = MLib("group", "id")
    val dep = MDependency(lib, "1.0", List(), Scope.COMPILE, true)

    val versions = List("0.2", "0.5", "1.3")

    assert(dep.filterVersions(versions) == Set(
      MPackage(lib, "1.0"),
      MPackage(lib, "1.3")
    ))
  }
}
