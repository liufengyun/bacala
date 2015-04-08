package bacala.test.maven

import bacala.test._
import bacala.maven._

class MavenRepositorySuite extends BasicSuite {
  test("should observe excludes in dependency") {
    val root = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>root</artifactId>
          <version>1.2</version>
          <dependencies>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>B</artifactId>
                  <version>1.0</version>
                  <exclusions>
                      <exclusion>
                          <groupId>org.test</groupId>
                          <artifactId>C</artifactId>
                      </exclusion>
                  </exclusions>
              </dependency>
          </dependencies>
      </project>
      """

    val b = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>B</artifactId>
          <version>1.0</version>
          <dependencies>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>C</artifactId>
                  <version>1.3</version>
              </dependency>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>D</artifactId>
                  <version>2.3</version>
              </dependency>
          </dependencies>
      </project>
      """

    val c = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>C</artifactId>
          <version>1.0</version>
          <dependencies>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>D</artifactId>
                  <version>2.3</version>
              </dependency>
          </dependencies>
      </project>
          """

    val d = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>D</artifactId>
          <version>2.3</version>
      </project>
          """

    val fetcher = (p: MavenPackage) => p match {
      case MavenPackage(MavenArtifact("org.test", "root"), _) => Some(root)
      case MavenPackage(MavenArtifact("org.test", "B"), _) => Some(b)
      case MavenPackage(MavenArtifact("org.test", "C"), _) => Some(c)
      case MavenPackage(MavenArtifact("org.test", "D"), _) => Some(d)
      case _ => None
    }

    val pom = MavenPomParser(root, null)
    val repo = new MavenRepository(pom) {
      override def makePomResolver(resolvers: Iterable[MavenResolver]) = {
        (pkg: MavenPackage) => fetcher(pkg).map(spec => MavenPomParser(spec, null))
      }

      override def makeMetaResolver(resolvers: Iterable[MavenResolver]) = {
        (pkg: MavenArtifact) => Some(Seq())
      }
    }

    // compile scope
    repo.construct(Scope.COMPILE)

    assert(repo(MavenPackage(MavenArtifact("org.test", "B"), "1.0")).toSet == Set(
      Set(MavenPackage(MavenArtifact("org.test", "D"), "2.3"))
    ))
    assert(repo.packages.size === 3)
  }

  test("should ignore optional and test dependencies") {
    val root = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>root</artifactId>
          <version>1.2</version>
          <dependencies>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>B</artifactId>
                  <version>1.0</version>
              </dependency>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>D</artifactId>
                  <version>2.3</version>
                  <scope>test</scope>
              </dependency>
          </dependencies>
      </project>
      """

    val b = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>B</artifactId>
          <version>1.0</version>
          <dependencies>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>C</artifactId>
                  <version>1.3</version>
              </dependency>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>D</artifactId>
                  <version>2.3</version>
                  <optional>true</optional>
              </dependency>
          </dependencies>
      </project>
      """

    val c = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>C</artifactId>
          <version>1.0</version>
          <dependencies>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>D</artifactId>
                  <version>2.3</version>
                  <scope>test</scope>
              </dependency>
          </dependencies>
      </project>
          """

    val d = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>D</artifactId>
          <version>2.3</version>
      </project>
          """

    val fetcher = (p: MavenPackage) => p match {
      case MavenPackage(MavenArtifact("org.test", "root"), _) => Some(root)
      case MavenPackage(MavenArtifact("org.test", "B"), _) => Some(b)
      case MavenPackage(MavenArtifact("org.test", "C"), _) => Some(c)
      case MavenPackage(MavenArtifact("org.test", "D"), _) => Some(d)
      case _ => None
    }

    val pom = MavenPomParser(root, null)
    val repo = new MavenRepository(pom) {
      override def makePomResolver(resolvers: Iterable[MavenResolver]) = {
        (pkg: MavenPackage) => fetcher(pkg).map(spec => MavenPomParser(spec, null))
      }

      override def makeMetaResolver(resolvers: Iterable[MavenResolver]) = {
        (pkg: MavenArtifact) => Some(Seq())
      }
    }

    // compile scope
    repo.construct(Scope.COMPILE)

    assert(repo(MavenPackage(MavenArtifact("org.test", "B"), "1.0")).toSet == Set(
      Set(MavenPackage(MavenArtifact("org.test", "C"), "1.3"))
    ))
    assert(repo.packages.size === 3)

    // test scope
    repo.reset
    repo.construct(Scope.TEST)

    assert(repo(MavenPackage(MavenArtifact("org.test", "B"), "1.0")).toSet == Set(
      Set(MavenPackage(MavenArtifact("org.test", "C"), "1.3"))
    ))
    assert(repo(MavenPackage(MavenArtifact("org.test", "C"), "1.3")).toSet == Set())
    assert(repo.packages.size === 4)
  }

  test("should pick up excludes in different path") {
    val root = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>root</artifactId>
          <version>1.2</version>
          <dependencies>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>C</artifactId>
                  <version>1.3</version>
              </dependency>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>B</artifactId>
                  <version>1.0</version>
                  <exclusions>
                      <exclusion>
                          <groupId>org.test</groupId>
                          <artifactId>C</artifactId>
                      </exclusion>
                  </exclusions>
              </dependency>
          </dependencies>
      </project>
      """

    val b = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>B</artifactId>
          <version>1.0</version>
          <dependencies>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>C</artifactId>
                  <version>1.3</version>
              </dependency>
          </dependencies>
      </project>
      """

    val c = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>C</artifactId>
          <version>1.3</version>
          <dependencies>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>D</artifactId>
                  <version>2.3</version>
              </dependency>
          </dependencies>
      </project>
          """

    val d = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>D</artifactId>
          <version>2.3</version>
      </project>
          """


    val fetcher = (p: MavenPackage) => p match {
      case MavenPackage(MavenArtifact("org.test", "root"), _) => Some(root)
      case MavenPackage(MavenArtifact("org.test", "B"), _) => Some(b)
      case MavenPackage(MavenArtifact("org.test", "C"), _) => Some(c)
      case MavenPackage(MavenArtifact("org.test", "D"), _) => Some(d)
      case _ => None
    }

    val pom = MavenPomParser(root, null)
    val repo = new MavenRepository(pom) {
      override def makePomResolver(resolvers: Iterable[MavenResolver]) = {
        (pkg: MavenPackage) => fetcher(pkg).map(spec => MavenPomParser(spec, null))
      }

      override def makeMetaResolver(resolvers: Iterable[MavenResolver]) = {
        (pkg: MavenArtifact) => Some(Seq())
      }
    }

    repo.construct(Scope.COMPILE)

    assert(repo(MavenPackage(MavenArtifact("org.test", "B"), "1.0")).toSet == Set())
    assert(repo.packages.size === 4)
  }

  test("should pick up excludes in different path 2") {
    val root = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>root</artifactId>
          <version>1.2</version>
          <dependencies>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>D</artifactId>
                  <version>2.3</version>
              </dependency>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>B</artifactId>
                  <version>1.0</version>
                  <exclusions>
                      <exclusion>
                          <groupId>org.test</groupId>
                          <artifactId>C</artifactId>
                      </exclusion>
                  </exclusions>
              </dependency>
          </dependencies>
      </project>
      """

    val b = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>B</artifactId>
          <version>1.0</version>
          <dependencies>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>C</artifactId>
                  <version>1.3</version>
              </dependency>
          </dependencies>
      </project>
      """

    val c = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>C</artifactId>
          <version>1.3</version>
          <dependencies>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>D</artifactId>
                  <version>2.3</version>
              </dependency>
          </dependencies>
      </project>
          """

    val d = """
      <project>
          <groupId>org.test</groupId>
          <artifactId>D</artifactId>
          <version>2.3</version>
          <dependencies>
              <dependency>
                  <groupId>org.test</groupId>
                  <artifactId>B</artifactId>
                  <version>1.0</version>
              </dependency>
          </dependencies>
      </project>
          """


    val fetcher = (p: MavenPackage) => p match {
      case MavenPackage(MavenArtifact("org.test", "root"), _) => Some(root)
      case MavenPackage(MavenArtifact("org.test", "B"), _) => Some(b)
      case MavenPackage(MavenArtifact("org.test", "C"), _) => Some(c)
      case MavenPackage(MavenArtifact("org.test", "D"), _) => Some(d)
      case _ => None
    }

    val pom = MavenPomParser(root, null)
    val repo = new MavenRepository(pom) {
      override def makePomResolver(resolvers: Iterable[MavenResolver]) = {
        (pkg: MavenPackage) => fetcher(pkg).map(spec => MavenPomParser(spec, null))
      }

      override def makeMetaResolver(resolvers: Iterable[MavenResolver]) = {
        (pkg: MavenArtifact) => Some(Seq())
      }
    }

    repo.construct(Scope.COMPILE)

    assert(repo(MavenPackage(MavenArtifact("org.test", "B"), "1.0")).toSet == Set(
      Set(MavenPackage(MavenArtifact("org.test", "C"), "1.3"))
    ))
    assert(repo.packages.size === 4)
  }

}
