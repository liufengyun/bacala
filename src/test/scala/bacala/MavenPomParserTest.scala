package bacala.test

import org.scalatest._
import bacala.maven._
import scala.xml.XML

class MavenPomParserSuite extends BasicSuite {

  test("parse POM with range version") {
    val pomFile = MavenPomParser(
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
      """, null)

    assert(pomFile.deps === Seq(
      MavenDependency(MavenArtifact("org.scala-lang", "scala-library"), "[2.11.3, 2.11.6)",   List[MavenArtifact](), Scope.COMPILE, false),
      MavenDependency(MavenArtifact("com.typesafe", "config"), "(1.1.1, 1.2.1]",   List[MavenArtifact](), Scope.COMPILE, false)
    ))
  }

  test("parse POM with dependency default version the same as package if they are the same group") {
    val pomFile = MavenPomParser(
      """
      <project>
          <groupId>org.test</groupId>
          <artifactId>test</artifactId>
          <version>2.4</version>
          <dependencies>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>config</artifactId>
              </dependency>
          </dependencies>
      </project>
      """, null)

    assert(pomFile.deps === Seq(
      MavenDependency(MavenArtifact("org.test", "config"), "2.4",   List[MavenArtifact](), Scope.COMPILE, false)
    ))
  }


  test("test new line and space in specification") {
    val pom = MavenPomParser(
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
      """, null)

    assert(pom.deps === Seq(
      MavenDependency(MavenArtifact("org.scala-lang", "scala-library"), "[2.11.3, 2.11.3]",   List[MavenArtifact](), Scope.COMPILE, false)
    ))
  }


  test("parse POM with composite range version") {
    val pom = MavenPomParser(
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
      """, null)

    assert(pom.deps === Seq(
      MavenDependency(MavenArtifact("org.scala-lang", "scala-library"), "(2.11.0, 2.11.3), (2.11.3, 2.11.6)",  List[MavenArtifact](), Scope.COMPILE, false),
      MavenDependency(MavenArtifact("com.typesafe", "config"), "(1.1.1, 1.2.1]", List[MavenArtifact](), Scope.COMPILE, false)
    ))
  }

  test("version defined in parent POM") {
    val fetcher = (p: MavenPackage) => p match {
      case MavenPackage(MavenArtifact("org.test", "parent"), "3.2") => Some(
        """
          <project>
              <groupId>org.test</groupId>
              <artifactId>parent</artifactId>
              <version>3.2</version>
              <dependencyManagement>
                  <dependencies>
                      <dependency>
                          <groupId>org.scala-lang</groupId>
                          <artifactId>scala-library</artifactId>
                          <version>(2.11.0, 2.11.3), (2.11.3, 2.11.6)</version>
                      </dependency>
                  </dependencies>
              </dependencyManagement>
          </project>
          """)
    }

    val pom = MavenPomParser(
      """
      <project>
          <groupId>org.test</groupId>
          <artifactId>test</artifactId>
          <version>2.4</version>
          <parent>
              <groupId>org.test</groupId>
              <artifactId>parent</artifactId>
              <version>3.2</version>
          </parent>
          <dependencies>
              <dependency>
                  <groupId>org.scala-lang</groupId>
                  <artifactId>scala-library</artifactId>
              </dependency>
          </dependencies>
      </project>
      """, fetcher)

    assert(pom.deps === Seq(
      MavenDependency(MavenArtifact("org.scala-lang", "scala-library"), "(2.11.0, 2.11.3), (2.11.3, 2.11.6)", List[MavenArtifact](), Scope.COMPILE, false)
    ))

  }

  test("dependencies defined in parent POM") {
    val parent = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>parent</artifactId>
          <version>3.2</version>
          <dependencies>
              <dependency>
                  <groupId>com.typesafe</groupId>
                  <artifactId>config</artifactId>
                  <version>(1.1.1, 1.2.1]</version>
              </dependency>
          </dependencies>
          <dependencyManagement>
              <dependencies>
                  <dependency>
                      <groupId>org.scala-lang</groupId>
                      <artifactId>scala-library</artifactId>
                      <version>(2.11.0, 2.11.3), (2.11.3, 2.11.6)</version>
                  </dependency>
              </dependencies>
          </dependencyManagement>
      </project>
      """

    val child = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>test</artifactId>
          <version>2.4</version>
          <parent>
              <groupId>org.test</groupId>
              <artifactId>parent</artifactId>
              <version>3.2</version>
          </parent>
          <dependencies>
              <dependency>
                  <groupId>org.scala-lang</groupId>
                  <artifactId>scala-library</artifactId>
              </dependency>
          </dependencies>
      </project>
      """

    val fetcher = (p: MavenPackage) => p match {
      case MavenPackage(MavenArtifact("org.test", "parent"), "3.2") => Some(parent)
    }

    val pom = MavenPomParser(child, fetcher)

    assert(pom.deps === Seq(
      MavenDependency(MavenArtifact("org.scala-lang", "scala-library"), "(2.11.0, 2.11.3), (2.11.3, 2.11.6)", List[MavenArtifact](), Scope.COMPILE, false),
      MavenDependency(MavenArtifact("com.typesafe", "config"), "(1.1.1, 1.2.1]", List[MavenArtifact](), Scope.COMPILE, false)
    ))
  }

  test("POM with multiple modules") {
    val parent = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>parent</artifactId>
          <version>2.4</version>
          <modules>
              <module>m1</module>
              <module>m2</module>
          </modules>
          <dependencies>
              <dependency>
                  <groupId>org.scala-lang</groupId>
                  <artifactId>scala-library</artifactId>
                  <version>1.6</version>
              </dependency>
          </dependencies>
      </project>
      """

    val m1 = """
          <project>
              <groupId>org.test</groupId>
              <artifactId>m1</artifactId>
              <version>2.4</version>
              <parent>
                  <groupId>org.test</groupId>
                  <artifactId>parent</artifactId>
                  <version>2.4</version>
              </parent>
              <dependencies>
                  <dependency>
                      <groupId>com.typesafe</groupId>
                      <artifactId>config</artifactId>
                      <version>(1.1.1, 1.2.1]</version>
                  </dependency>
              </dependencies>
          </project>
          """

    val m2 = """
          <project>
              <groupId>org.test</groupId>
              <artifactId>m2</artifactId>
              <version>2.4</version>
              <parent>
                  <groupId>org.test</groupId>
                  <artifactId>parent</artifactId>
                  <version>2.4</version>
              </parent>
              <dependencies>
                  <dependency>
                      <groupId>com.typesafe</groupId>
                      <artifactId>slick</artifactId>
                      <version>(1.1.1, 1.2.1]</version>
                  </dependency>
              </dependencies>
          </project>
          """

    val fetcher = (p: MavenPackage) => p match {
      case MavenPackage(MavenArtifact("org.test", "parent"), "2.4") => Some(parent)
      case MavenPackage(MavenArtifact("org.test", "m1"), "2.4") => Some(m1)
      case MavenPackage(MavenArtifact("org.test", "m2"), "2.4") => Some(m2)
      case _ => None
    }

    val pom = MavenPomParser(parent, fetcher)

    assert(pom.deps.toSet === Set(
      MavenDependency(MavenArtifact("org.scala-lang", "scala-library"), "1.6", List[MavenArtifact](), Scope.COMPILE, false),
      MavenDependency(MavenArtifact("com.typesafe", "config"), "(1.1.1, 1.2.1]", List[MavenArtifact](), Scope.COMPILE, false),
      MavenDependency(MavenArtifact("com.typesafe", "slick"), "(1.1.1, 1.2.1]", List[MavenArtifact](), Scope.COMPILE, false)
    ))
  }

  test("child POM will not inherit modules in parent POM") {
    val parent = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>parent</artifactId>
          <version>2.4</version>
          <modules>
              <module>m1</module>
              <module>m2</module>
          </modules>
          <dependencies>
              <dependency>
                  <groupId>org.scala-lang</groupId>
                  <artifactId>scala-library</artifactId>
                  <version>1.6</version>
              </dependency>
          </dependencies>
      </project>
      """

    val child = """
          <project>
              <groupId>org.test</groupId>
              <artifactId>m1</artifactId>
              <version>2.4</version>
              <parent>
                  <groupId>org.test</groupId>
                  <artifactId>parent</artifactId>
                  <version>2.4</version>
              </parent>
              <dependencies>
                  <dependency>
                      <groupId>com.typesafe</groupId>
                      <artifactId>config</artifactId>
                      <version>(1.1.1, 1.2.1]</version>
                  </dependency>
              </dependencies>
          </project>
          """

    val fetcher = (p: MavenPackage) => p match {
      case MavenPackage(MavenArtifact("org.test", "parent"), "2.4") => Some(parent)
      case MavenPackage(MavenArtifact("org.test", "m1"), "2.4") => Some(child)
      case _ => None
    }

    val pom = MavenPomParser(child, fetcher)

    assert(pom.deps.toSet === Set(
      MavenDependency(MavenArtifact("org.scala-lang", "scala-library"), "1.6", List[MavenArtifact](), Scope.COMPILE, false),
      MavenDependency(MavenArtifact("com.typesafe", "config"), "(1.1.1, 1.2.1]", List[MavenArtifact](), Scope.COMPILE, false)
    ))
  }

  test("test parent chain") {
    val grandParent = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>parentParent</artifactId>
          <version>1.2</version>
          <dependencies>
              <dependency>
                  <groupId>org.scala-lang</groupId>
                  <artifactId>test-lib</artifactId>
                  <version>1.1.1</version>
              </dependency>
          </dependencies>
          <modules>
              <module>m1</module>
              <module>m2</module>
          </modules>
      </project>
      """

    val parent = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>parent</artifactId>
          <version>2.4</version>
          <parent>
              <groupId>org.test</groupId>
              <artifactId>grandParent</artifactId>
              <version>1.2</version>
          </parent>
          <modules>
              <module>m1</module>
              <module>m2</module>
          </modules>
          <dependencies>
              <dependency>
                  <groupId>org.scala-lang</groupId>
                  <artifactId>scala-library</artifactId>
                  <version>1.6</version>
              </dependency>
          </dependencies>
      </project>
      """

    val child = """
          <project>
              <groupId>org.test</groupId>
              <artifactId>m1</artifactId>
              <version>2.4</version>
              <parent>
                  <groupId>org.test</groupId>
                  <artifactId>parent</artifactId>
                  <version>2.4</version>
              </parent>
              <dependencies>
                  <dependency>
                      <groupId>com.typesafe</groupId>
                      <artifactId>config</artifactId>
                      <version>(1.1.1, 1.2.1]</version>
                  </dependency>
              </dependencies>
          </project>
          """

    val fetcher = (p: MavenPackage) => p match {
      case MavenPackage(MavenArtifact("org.test", "grandParent"), "1.2") => Some(grandParent)
      case MavenPackage(MavenArtifact("org.test", "parent"), "2.4") => Some(parent)
      case MavenPackage(MavenArtifact("org.test", "m1"), "2.4") => Some(child)
      case _ => None
    }

    val pom = MavenPomParser(child, fetcher)

    assert(pom.deps.toSet === Set(
      MavenDependency(MavenArtifact("org.scala-lang", "test-lib"), "1.1.1", List[MavenArtifact](), Scope.COMPILE, false),
      MavenDependency(MavenArtifact("org.scala-lang", "scala-library"), "1.6", List[MavenArtifact](), Scope.COMPILE, false),
      MavenDependency(MavenArtifact("com.typesafe", "config"), "(1.1.1, 1.2.1]", List[MavenArtifact](), Scope.COMPILE, false)
    ))
  }

  test("POM with nested modules") {
    val parent = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>parent</artifactId>
          <version>2.4</version>
          <modules>
              <module>m1</module>
          </modules>
          <dependencies>
              <dependency>
                  <groupId>org.scala-lang</groupId>
                  <artifactId>scala-library</artifactId>
                  <version>1.6</version>
              </dependency>
          </dependencies>
      </project>
      """

    val m1 = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>m1</artifactId>
          <version>2.4</version>
          <parent>
              <groupId>org.test</groupId>
              <artifactId>parent</artifactId>
              <version>2.4</version>
          </parent>
          <modules>
              <module>m2</module>
          </modules>
          <dependencies>
              <dependency>
                  <groupId>com.typesafe</groupId>
                  <artifactId>config</artifactId>
                  <version>(1.1.1, 1.2.1]</version>
              </dependency>
          </dependencies>
      </project>
      """

    val m2 = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>m2</artifactId>
          <version>2.4</version>
          <parent>
              <groupId>org.test</groupId>
              <artifactId>parent</artifactId>
              <version>2.4</version>
          </parent>
          <dependencies>
              <dependency>
                  <groupId>com.typesafe</groupId>
                  <artifactId>slick</artifactId>
                  <version>(1.1.1, 1.2.1]</version>
              </dependency>
          </dependencies>
      </project>
          """

    val fetcher = (p: MavenPackage) => p match {
      case MavenPackage(MavenArtifact("org.test", "parent"), "2.4") => Some(parent)
      case MavenPackage(MavenArtifact("org.test", "m1"), "2.4") => Some(m1)
      case MavenPackage(MavenArtifact("org.test", "m2"), "2.4") => Some(m2)
      case _ => None
    }

    val pom = MavenPomParser(parent, fetcher)

    assert(pom.deps.toSet === Set(
      MavenDependency(MavenArtifact("org.scala-lang", "scala-library"), "1.6", List[MavenArtifact](), Scope.COMPILE, false),
      MavenDependency(MavenArtifact("com.typesafe", "config"), "(1.1.1, 1.2.1]", List[MavenArtifact](), Scope.COMPILE, false),
      MavenDependency(MavenArtifact("com.typesafe", "slick"), "(1.1.1, 1.2.1]", List[MavenArtifact](), Scope.COMPILE, false)
    ))
  }


  test("parse POM with unspecified version") {
    val pom = MavenPomParser(
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
      """, null)

    assert(pom.deps === Seq(
      MavenDependency(MavenArtifact("org.scala-lang", "scala-library"), "0.0.0", List[MavenArtifact](), Scope.COMPILE, false)
    ))
  }

  test("parse POM with groupId in parent section") {
    val fetcher = (p: MavenPackage) => p match {
      case MavenPackage(MavenArtifact("org.test", "parent"), "3.2") => Some(
        """
          <project>
              <groupId>org.test</groupId>
              <artifactId>parent</artifactId>
              <version>3.2</version>
          </project>
          """)
    }

    val pom = MavenPomParser(
      """
      <project>
          <artifactId>test</artifactId>
          <parent>
              <groupId>org.test</groupId>
              <artifactId>parent</artifactId>
              <version>3.2</version>
          </parent>
          <dependencies>
              <dependency>
                  <groupId>${project.groupId}</groupId>
                  <artifactId>scala-library</artifactId>
                  <version>3.2</version>
              </dependency>
          </dependencies>
      </project>
      """, fetcher)

    assert(pom.deps === Seq(
      MavenDependency(MavenArtifact("org.test", "scala-library"), "3.2", List[MavenArtifact](), Scope.COMPILE, false)
    ))

  }

  test("parse resolvers") {
    val pom = MavenPomParser(
      """
      <project>
          <groupId>org.test</groupId>
          <artifactId>test</artifactId>
          <version>2.4</version>
          <dependencies>
              <dependency>
                  <groupId>org.scala-lang</groupId>
                  <artifactId>scala-library</artifactId>
                  <version>2.11.3</version>
              </dependency>
          </dependencies>
         <repositories>
             <repository>
                 <id>codehaus-snapshots</id>
                 <name>Codehaus Snapshots</name>
                 <url>http://repository.codehaus.org/</url>
             </repository>
             <repository>
                 <id>lucene-snapshots</id>
                 <name>Lucene Snapshots</name>
                 <url>https://download.elastic.co/lucenesnapshots/1662607</url>
             </repository>
         </repositories>
      </project>
      """, null)

    assert(pom.resolvers.toSet === Set(
      MavenResolver("codehaus-snapshots", "Codehaus Snapshots", "http://repository.codehaus.org/"),
      MavenResolver("lucene-snapshots", "Lucene Snapshots", "https://download.elastic.co/lucenesnapshots/1662607")
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
    assert(Property.resolve(XML.loadString(xml), "x") === Some("56"))
  }

  test("test variable property with dot in name") {
    val xml = """<project><properties><x.y>56</x.y></properties></project>"""
    assert(Property.unapply("${x.y}").nonEmpty)
    assert(Property.resolve(XML.loadString(xml), "x.y") === Some("56"))
  }

  test("test  property reference another property") {
    val xml = """<project><properties><x>56</x><y>${x}</y></properties></project>"""
    assert(Property.resolve(XML.loadString(xml), "y") === Some("56"))
  }

  test("test variable property with dash in name") {
    val xml = """<project><properties><x-y>56</x-y></properties></project>"""
    assert(Property.unapply("${x-y}").nonEmpty)
    assert(Property.resolve(XML.loadString(xml), "x-y") === Some("56"))
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

    assert(Property.resolve(XML.loadString(xml), "project.version") === Some("4.3"))
  }

  test("test old-style path property") {
    val xml = """<project><version>56</version><artifactId>test-artifact</artifactId></project>"""
    assert(Property.resolve(XML.loadString(xml), "version") === Some("56"))
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

    assert(Property.resolve(XML.loadString(xml), "project.version") === Some("4.3"))
  }

  test("test version resolution via parent") {
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

    assert(PomFile.resolveVersion(XML.loadString(xml)) === "4.3")
  }

  test("test groupId resolution via parent") {
    val xml =   """
      <project>
          <parent>
              <groupId>org.test</groupId>
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

    assert(PomFile.resolveGroupId(XML.loadString(xml)) === "org.test")
  }
}
