package bacala.test.core

import bacala.test._
import bacala.core._

class VersionRangeSuite extends BasicSuite {
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

    assert(VersionRange("[1.0]") ===
      IntervalRange(Version(1, 0, 0, "", 0), Version(1, 0, 0, "", 0), true,  true))
  }

  test("composite range") {
    assert(VersionRange("(,1.0], (1.2,)") === CompositeRange(List(
      OpenLeftRange(Version(1, 0, 0, "", 0), true),
      OpenRightRange(Version(1, 2, 0, "", 0), false)
    )))
  }
}
