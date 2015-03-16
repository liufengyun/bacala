package bacala.test

import org.scalatest._
import bacala.maven._

class MavenPomParserSuite extends BasicSuite {
  test("parse POM with exact version") {
    val deps = MavenPomParser(
      """
      <project>
          <dependencies>
              <dependency>
                  <groupId>org.scala-lang</groupId>
                  <artifactId>scala-library</artifactId>
                  <version>2.11.5</version>
              </dependency>
              <dependency>
                  <groupId>com.typesafe</groupId>
                  <artifactId>config</artifactId>
                  <version>1.2.1</version>
              </dependency>
          </dependencies>
      </project>
      """)

    assert(deps === Set(
      Set(MavenPackage("org.scala-lang", "scala-library", "2.11.5")),
      Set(MavenPackage("com.typesafe", "config", "1.2.1"))
    ))
  }
}