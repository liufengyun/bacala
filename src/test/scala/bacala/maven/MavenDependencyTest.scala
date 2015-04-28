package bacala.test.maven

import bacala.test._
import bacala.maven._
import bacala.core.{JLib, JPackage}

class MavenDependencySuite extends BasicSuite {
  test("compatible set should include the specified version") {
    val lib = JLib("group", "id")
    val dep = MavenDependency(lib, "1.0", List(), Scope.COMPILE, true)

    val versions = List("0.2", "0.5", "1.3")

    assert(dep.resolve(versions) == Set(
      JPackage(lib, "1.0"),
      JPackage(lib, "1.3")
    ))
  }
}
