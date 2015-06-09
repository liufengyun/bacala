package bacala.test.ivy

import bacala.test._
import bacala.ivy._

class DataSuite extends BasicSuite {
  val descriptor = IDescriptor(null, Map("compile" -> IConf("compile", "", "", Seq(), true, ""),
    "test" -> IConf("test", "", "", Seq(), true, "")), Seq(), Map(), Set(), null)

  test("getExtendedConfigurations correctly handle compile") {
    assert(descriptor.getExtendedConfigurations(Set("compile")) === Set("compile"))
  }

  test("getExtendedConfigurations correctly handle compile(*)") {
    assert(descriptor.getExtendedConfigurations(Set("compile(*)")) === Set("compile"))
  }

  test("getExtendedConfigurations correctly handle none(*)") {
    assert(descriptor.getExtendedConfigurations(Set("none(*)")) === Set("compile", "test"))
  }
}
