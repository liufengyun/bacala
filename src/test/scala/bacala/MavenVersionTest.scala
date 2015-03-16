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

  test("unstandard druple") {
    assert(Version("2.7.3.RC1") === Version(2, 7, 3, "RC1", 0))
  }

  test("wildcard") {
    assert(Version("2.8.0.Beta1-RC1") === Version(2, 8, 0, "Beta1-RC1", 0))
    assert(Version("2.10.0-M1-virtualized.rdev-4217-2012-01-24-g9118644") === Version(2, 10, 0, "M1-virtualized.rdev-4217-2012-01-24-g9118644", 0))
  }

  test("Vesion comparison") {
    assert(Version("3.6.2-RC4-4") < Version("3.6.2-RC4-5"))
    assert(Version("3.6") > Version("3.5.2-RC4-5"))
    assert(Version("3.5.2-RC4-5") < Version("3.5.2-TC4-5"))
    assert(Version("2.10.3-RC2") < Version("2.10.3"))
  }

  test("simple range") {
    assert(VersionRange("3.6.2-RC4") === SimpleRange(Version(3, 6, 2, "RC4", 0)))
  }

  test("open left range") {
    assert(VersionRange("(,1.0]") === OpenLeftRange(Version(1, 0, 0, "", 0), true))
    assert(VersionRange("(,2.3)") === OpenLeftRange(Version(2, 3, 0, "", 0), false))
  }

  test("open right range") {
    assert(VersionRange("[1.0,)") === OpenRightRange(Version(1, 0, 0, "", 0), true))
    assert(VersionRange("(2.3,)") === OpenRightRange(Version(2, 3, 0, "", 0), false))
  }

  test("interval range") {
    assert(VersionRange("[1.0, 2.0)") ===
      IntervalRange(Version(1, 0, 0, "", 0), Version(2, 0, 0, "", 0), true, false))

    assert(VersionRange("[1.0, 2.0]") ===
      IntervalRange(Version(1, 0, 0, "", 0), Version(2, 0, 0, "", 0), true, true))

    assert(VersionRange("(1.0, 2.0]") ===
      IntervalRange(Version(1, 0, 0, "", 0), Version(2, 0, 0, "", 0), false, true))

    assert(VersionRange("(1.0, 2.0)") ===
      IntervalRange(Version(1, 0, 0, "", 0), Version(2, 0, 0, "", 0), false,  false))
  }

  test("composite range") {
    assert(VersionRange("(,1.0], (1.2,)") === CompositeRange(List(
      OpenLeftRange(Version(1, 0, 0, "", 0), true),
      OpenRightRange(Version(1, 2, 0, "", 0), false)
    )))
  }
}
