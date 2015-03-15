package bacala.test

import org.scalatest._
import bacala.maven._

class MavenVersionSuite extends BasicSuite {

  test("simple versioning with major & minor") {
    assert(Version("10.5") === Version(10, 5, 0, "", 0))
  }

  test("triple (major, minor, revision)") {
    assert(Version("3.6.2") === Version(3, 6, 2, "", 0))
  }

  test("versioning with build") {
    assert(Version("3.6.2-78") === Version(3, 6, 2, "", 78))
  }

  test("versioning with qualifier") {
    assert(Version("3.6.2-RC4") === Version(3, 6, 2, "RC4", 0))
  }

  test("full versioning") {
    assert(Version("3.6.2-RC4-4") === Version(3, 6, 2, "RC4", 4))
  }

  test("Vesion comparison") {
    assert(Version("3.6.2-RC4-4") < Version("3.6.2-RC4-5"))
    assert(Version("3.6") > Version("3.5.2-RC4-5"))
    assert(Version("3.5.2-RC4-5") < Version("3.5.2-TC4-5"))
  }
}
