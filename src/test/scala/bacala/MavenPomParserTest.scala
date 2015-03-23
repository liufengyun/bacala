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
      """,
      Scope.COMPILE)

    assert(deps === Set(
      Set(
        MavenPackage("org.scala-lang", "scala-library", "2.11.3"),
        MavenPackage("org.scala-lang", "scala-library", "2.11.4"),
        MavenPackage("org.scala-lang", "scala-library", "2.11.5")
      ),
      Set(
        MavenPackage("com.typesafe", "config", "1.2.0"),
        MavenPackage("com.typesafe", "config", "1.2.1")
      )
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
      """,
      Scope.COMPILE)

    assert(deps === Set(
      Set(
        MavenPackage("org.scala-lang", "scala-library", "2.11.1"),
        MavenPackage("org.scala-lang", "scala-library", "2.11.2"),
        MavenPackage("org.scala-lang", "scala-library", "2.11.4"),
        MavenPackage("org.scala-lang", "scala-library", "2.11.5")
      ),
      Set(
        MavenPackage("com.typesafe", "config", "1.2.0"),
        MavenPackage("com.typesafe", "config", "1.2.1")
      )
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
      """,
      Scope.COMPILE)

    assert(deps.exists { set =>
      Set(
        MavenPackage("org.scala-lang", "scala-library", "2.11.1"),
        MavenPackage("org.scala-lang", "scala-library", "2.11.2"),
        MavenPackage("org.scala-lang", "scala-library", "2.11.4"),
        MavenPackage("org.scala-lang", "scala-library", "2.11.5")
      ).subsetOf(set)
    })
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
    assert(Property.resolve(XML.loadString(xml))("x.y") === "56")
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
