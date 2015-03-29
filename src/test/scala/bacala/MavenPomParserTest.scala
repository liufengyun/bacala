package bacala.test

import org.scalatest._
import bacala.maven._
import scala.xml.XML

class MavenPomParserSuite extends BasicSuite {
  test("parse POM with range version") {
    val deps = MavenPomParser(
      """
      <project>
          <groupId>org.test</groupId>
          <artifactId>test</artifactId>
          <version>2.4</version>
          <dependencies>
              <dependency>
                  <groupId>org.scala-lang</groupId>
                  <artifactId>scala-library</artifactId>
                  <version>[2.11.3, 2.11.6)</version>
              </dependency>
              <dependency>
                  <groupId>com.typesafe</groupId>
                  <artifactId>config</artifactId>
                  <version>(1.1.1, 1.2.1]</version>
              </dependency>
          </dependencies>
      </project>
      """)

    assert(deps === Seq(
      MavenDependency("org.scala-lang", "scala-library", VersionRange("[2.11.3, 2.11.6)"),   List[Exclusion](), Scope.COMPILE, false),
      MavenDependency("com.typesafe", "config", VersionRange("(1.1.1, 1.2.1]"),   List[Exclusion](), Scope.COMPILE, false)
    ))
  }

  test("test new line and space in specification") {
    val deps = MavenPomParser(
      """
      <project>
          <groupId>org.test</groupId>
          <artifactId>test</artifactId>
          <version>2.4</version>
          <dependencies>
              <dependency>
                  <groupId>  org.scala-lang </groupId>
                  <artifactId>
scala-library
</artifactId>
                  <version>
[2.11.3, 2.11.3]
</version>
              </dependency>
          </dependencies>
      </project>
      """)

    assert(deps === Seq(
      MavenDependency("org.scala-lang", "scala-library", VersionRange("[2.11.3, 2.11.3]"),   List[Exclusion](), Scope.COMPILE, false)
    ))
  }


  test("parse POM with composite range version") {
    val deps = MavenPomParser(
      """
      <project>
          <groupId>org.test</groupId>
          <artifactId>test</artifactId>
          <version>2.4</version>
          <dependencies>
              <dependency>
                  <groupId>org.scala-lang</groupId>
                  <artifactId>scala-library</artifactId>
                  <version>(2.11.0, 2.11.3), (2.11.3, 2.11.6)</version>
              </dependency>
              <dependency>
                  <groupId>com.typesafe</groupId>
                  <artifactId>config</artifactId>
                  <version>(1.1.1, 1.2.1]</version>
              </dependency>
          </dependencies>
      </project>
      """)

    assert(deps === Seq(
      MavenDependency("org.scala-lang", "scala-library", VersionRange("(2.11.0, 2.11.3), (2.11.3, 2.11.6)"),   List[Exclusion](), Scope.COMPILE, false),
      MavenDependency("com.typesafe", "config", VersionRange("(1.1.1, 1.2.1]"),   List[Exclusion](), Scope.COMPILE, false)
    ))
  }

  test("parse POM with unspecified version") {
    val deps = MavenPomParser(
      """
      <project>
          <groupId>org.test</groupId>
          <artifactId>test</artifactId>
          <version>2.4</version>
          <dependencies>
              <dependency>
                  <groupId>org.scala-lang</groupId>
                  <artifactId>scala-library</artifactId>
              </dependency>
          </dependencies>
      </project>
      """)

    assert(deps === Seq(
      MavenDependency("org.scala-lang", "scala-library", VersionRange("0.0.0"),   List[Exclusion](), Scope.COMPILE, false)
    ))
  }

  test("version specification via property") {
    assert(Property.unapply("${project.version}").nonEmpty)
    assert(Property.unapply("${project.version}").get === "project.version")
    assert(Property.unapply("  ${project.version}").get === "project.version")
    assert(Property.unapply("${project.version}  ").get === "project.version")
    assert(Property.unapply("${  project.version}  ").get === "project.version")
    assert(Property.unapply("${project.version  }  ").get === "project.version")
  }

  test("test variable property") {
    val xml = """<project><properties><x>56</x></properties></project>"""
    assert(Property.resolve(XML.loadString(xml))("x") === "56")
  }

  test("test variable property with dot in name") {
    val xml = """<project><properties><x.y>56</x.y></properties></project>"""
    assert(Property.unapply("${x.y}").nonEmpty)
    assert(Property.resolve(XML.loadString(xml))("x.y") === "56")
  }

  test("test variable property with dash in name") {
    val xml = """<project><properties><x-y>56</x-y></properties></project>"""
    assert(Property.unapply("${x-y}").nonEmpty)
    assert(Property.resolve(XML.loadString(xml))("x-y") === "56")
  }


  test("test path property resolution") {
    val xml =   """
      <project>
          <version>4.3</version>
          <dependencies>
              <dependency>
                  <groupId>com.typesafe</groupId>
                  <artifactId>config</artifactId>
                  <version>${project.version}</version>
              </dependency>
          </dependencies>
      </project>
      """

    assert(Property.resolve(XML.loadString(xml))("project.version") === "4.3")
  }

  test("test old-style path property") {
    val xml = """<project><version>56</version><artifactId>test-artifact</artifactId></project>"""
    assert(Property.resolve(XML.loadString(xml))("version") === "56")
  }

  test("test path property resolution via parent") {
    val xml =   """
      <project>
          <parent>
              <version>4.3</version>
          </parent>
          <dependencies>
              <dependency>
                  <groupId>com.typesafe</groupId>
                  <artifactId>config</artifactId>
                  <version>${project.version}</version>
              </dependency>
          </dependencies>
      </project>
      """

    assert(Property.resolve(XML.loadString(xml))("project.version") === "4.3")
  }
}
