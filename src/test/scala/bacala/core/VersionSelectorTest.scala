package bacala.test.core

import bacala.test._
import bacala.core._

class VersionSelectorSuite extends BasicSuite {
  val versions = """1.1.0-RC0, 1.1.0-RC1, 1.1.0-beta0, 1.1.0, 1.2, 1.3.0, 1.3.1, 1.4.0, 1.4.1, 1.4.2, 1.4.3, 1.5.0-M0, 1.5.0, 1.5.2, 1.5.3, 1.5.4, 1.5.5, 1.5.6, 1.5.7, 1.5.8, 1.5.9-RC0, 1.5.9.RC1, 1.5.10, 1.5.11, 1.6.0-RC0, 1.6.0-alpha2, 1.6.0, 1.6.1, 1.6.2, 1.6.3, 1.6.4, 1.6.5, 1.6.6, 1.7.0, 1.7.1, 1.7.2, 1.7.3, 1.7.4, 1.7.5, 1.7.6, 1.7.7, 1.7.8, 1.7.9, 1.7.10, 1.7.11, 1.7.12""".split(",").map(_.trim)

  test("default range selector") {
    VersionSelector("1.6.2").choose(versions).toSet == Set("1.6.2, 1.6.3, 1.6.4, 1.6.5, 1.6.6, 1.7.0, 1.7.1, 1.7.2, 1.7.3, 1.7.4, 1.7.5, 1.7.6, 1.7.7, 1.7.8, 1.7.9, 1.7.10, 1.7.11, 1.7.12".split(",").map(_.trim))
  }

  test("range selector") {
    assert(VersionSelector("[1.6.2, 1.7.5]").choose(versions).toSet === "1.6.2, 1.6.3, 1.6.4, 1.6.5, 1.6.6, 1.7.0, 1.7.1, 1.7.2, 1.7.3, 1.7.4, 1.7.5".split(",").map(_.trim).toSet)
    assert(VersionSelector("]1.6.2, 1.7.5]").choose(versions).toSet === "1.6.3, 1.6.4, 1.6.5, 1.6.6, 1.7.0, 1.7.1, 1.7.2, 1.7.3, 1.7.4, 1.7.5".split(",").map(_.trim).toSet)
    assert(VersionSelector("[1.6.2, 1.7.5[").choose(versions).toSet === "1.6.2, 1.6.3, 1.6.4, 1.6.5, 1.6.6, 1.7.0, 1.7.1, 1.7.2, 1.7.3, 1.7.4".split(",").map(_.trim).toSet)
    assert(VersionSelector("]1.6.2, 1.7.5[").choose(versions).toSet === "1.6.3, 1.6.4, 1.6.5, 1.6.6, 1.7.0, 1.7.1, 1.7.2, 1.7.3, 1.7.4".split(",").map(_.trim).toSet)
  }

  test("latest selector") {
    assert(VersionSelector("latest.release") === LatestSelector)
    assert(VersionSelector("latest.integration").choose(versions).toSet === Set("1.7.12"))
  }

  test("prefix latest selector") {
    assert(VersionSelector("1.6.+") === RangeLatestSelector("1.6"))
    assert(VersionSelector("1.6.+").choose(versions).toSet === Set("1.6.6"))
    assert(VersionSelector("1.+").choose(versions).toSet === Set("1.7.12"))
  }
}
