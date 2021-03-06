package bacala.test.maven

import bacala.test._
import bacala.maven._

class MavenRepositorySuite extends BasicSuite {
  test("missing dependency should corresponds to an empty set") {
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

    val fetcher = (p: MPackage) => p match {
      case MPackage(MLib("org.test", "root"), _) => Some(root)
      case MPackage(MLib("org.test", "B"), _) => Some(b)
      case MPackage(MLib("org.test", "C"), _) => Some(c)
      case _ => None
    }

    val pom = MavenPomParser(root, null)
    val repo = new MavenRepository(pom) {
      override def makeResolver(resolvers: Iterable[MResolver]) = {
        ((pkg: MPackage) => fetcher(pkg).map(spec => MavenPomParser(spec, null)),
        (pkg: MLib) => pkg match {
          case MLib(_, "B") => Some(Seq("1.0"))
          case MLib(_, "C") => Some(Seq("1.3"))
          case _ => None
        })
      }
    }

    // compile scope
    repo.construct(Scope.COMPILE)

    assert(repo(MPackage(MLib("org.test", "B"), "1.0")).map(_._2).toSet == Set(
      Set(MPackage(MLib("org.test", "C"), "1.3")),
      Set()
    ))
    assert(repo.packages.size === 2)
  }

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

    val fetcher = (p: MPackage) => p match {
      case MPackage(MLib("org.test", "root"), _) => Some(root)
      case MPackage(MLib("org.test", "B"), _) => Some(b)
      case MPackage(MLib("org.test", "C"), _) => Some(c)
      case MPackage(MLib("org.test", "D"), _) => Some(d)
      case _ => None
    }

    val pom = MavenPomParser(root, null)
    val repo = new MavenRepository(pom) {
      override def makeResolver(resolvers: Iterable[MResolver]) = {
        ((pkg: MPackage) => fetcher(pkg).map(spec => MavenPomParser(spec, null)),
          (pkg: MLib) => Some(Seq()))
      }
    }

    // compile scope
    repo.construct(Scope.COMPILE)

    assert(repo(MPackage(MLib("org.test", "B"), "1.0")).map(_._2).toSet == Set(
      Set(MPackage(MLib("org.test", "D"), "2.3"))
    ))
    assert(repo.packages.size === 2)
  }

  test("pattern to exclude all transitive dependencies via *") {
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
                          <groupId>*</groupId>
                          <artifactId>*</artifactId>
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

    val fetcher = (p: MPackage) => p match {
      case MPackage(MLib("org.test", "root"), _) => Some(root)
      case MPackage(MLib("org.test", "B"), _) => Some(b)
      case MPackage(MLib("org.test", "C"), _) => Some(c)
      case MPackage(MLib("org.test", "D"), _) => Some(d)
      case _ => None
    }

    val pom = MavenPomParser(root, null)
    val repo = new MavenRepository(pom) {
      override def makeResolver(resolvers: Iterable[MResolver]) = {
        ((pkg: MPackage) => fetcher(pkg).map(spec => MavenPomParser(spec, null)),
          (pkg: MLib) => Some(Seq()))
      }
    }

    // compile scope
    repo.construct(Scope.COMPILE)

    assert(repo(MPackage(MLib("org.test", "B"), "1.0")).toSet == Set())
    assert(repo.packages.size === 1)
  }

  test("pattern to exclude all transitive dependencies of a group via *") {
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
                          <groupId>org.exclude</groupId>
                          <artifactId>*</artifactId>
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
                  <groupId>org.exclude</groupId>
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
          <groupId>org.exclude</groupId>
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

    val fetcher = (p: MPackage) => p match {
      case MPackage(MLib("org.test", "root"), _) => Some(root)
      case MPackage(MLib("org.test", "B"), _) => Some(b)
      case MPackage(MLib("org.exclude", "C"), _) => Some(c)
      case MPackage(MLib("org.test", "D"), _) => Some(d)
      case _ => None
    }

    val pom = MavenPomParser(root, null)
    val repo = new MavenRepository(pom) {
      override def makeResolver(resolvers: Iterable[MResolver]) = {
        ((pkg: MPackage) => fetcher(pkg).map(spec => MavenPomParser(spec, null)),
          (pkg: MLib) => Some(Seq()))
      }
    }

    // compile scope
    repo.construct(Scope.COMPILE)

    assert(repo(MPackage(MLib("org.test", "B"), "1.0")).map(_._2).toSet == Set(
      Set(MPackage(MLib("org.test", "D"), "2.3"))
    ))
    assert(repo.packages.size === 2)
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

    val fetcher = (p: MPackage) => p match {
      case MPackage(MLib("org.test", "root"), _) => Some(root)
      case MPackage(MLib("org.test", "B"), _) => Some(b)
      case MPackage(MLib("org.test", "C"), _) => Some(c)
      case MPackage(MLib("org.test", "D"), _) => Some(d)
      case _ => None
    }

    val pom = MavenPomParser(root, null)
    val repo = new MavenRepository(pom) {
      override def makeResolver(resolvers: Iterable[MResolver]) = {
        ((pkg: MPackage) => fetcher(pkg).map(spec => MavenPomParser(spec, null)),
          (pkg: MLib) => Some(Seq()))
      }
    }

    // compile scope
    repo.construct(Scope.COMPILE)

    assert(repo(MPackage(MLib("org.test", "B"), "1.0")).map(_._2).toSet == Set(
      Set(MPackage(MLib("org.test", "C"), "1.3"))
    ))
    assert(repo.packages.size === 2)

    // test scope
    repo.construct(Scope.TEST)

    assert(repo(MPackage(MLib("org.test", "B"), "1.0")).map(_._2).toSet == Set(
      Set(MPackage(MLib("org.test", "C"), "1.3"))
    ))
    assert(repo(MPackage(MLib("org.test", "C"), "1.3")).map(_._2).toSet == Set())
    assert(repo.packages.size === 3)
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


    val fetcher = (p: MPackage) => p match {
      case MPackage(MLib("org.test", "root"), _) => Some(root)
      case MPackage(MLib("org.test", "B"), _) => Some(b)
      case MPackage(MLib("org.test", "C"), _) => Some(c)
      case MPackage(MLib("org.test", "D"), _) => Some(d)
      case _ => None
    }

    val pom = MavenPomParser(root, null)
    val repo = new MavenRepository(pom) {
      override def makeResolver(resolvers: Iterable[MResolver]) = {
        ((pkg: MPackage) => fetcher(pkg).map(spec => MavenPomParser(spec, null)),
          (pkg: MLib) => Some(Seq()))
      }
    }

    repo.construct(Scope.COMPILE)

    assert(repo(MPackage(MLib("org.test", "B"), "1.0")).toSet == Set())
    assert(repo.packages.size === 3)
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


    val fetcher = (p: MPackage) => p match {
      case MPackage(MLib("org.test", "root"), _) => Some(root)
      case MPackage(MLib("org.test", "B"), _) => Some(b)
      case MPackage(MLib("org.test", "C"), _) => Some(c)
      case MPackage(MLib("org.test", "D"), _) => Some(d)
      case _ => None
    }

    val pom = MavenPomParser(root, null)
    val repo = new MavenRepository(pom) {
      override def makeResolver(resolvers: Iterable[MResolver]) = {
        ((pkg: MPackage) => fetcher(pkg).map(spec => MavenPomParser(spec, null)),
          (pkg: MLib) => Some(Seq()))
      }
    }

    repo.construct(Scope.COMPILE)

    assert(repo(MPackage(MLib("org.test", "B"), "1.0")).map(_._2).toSet == Set(
      Set(MPackage(MLib("org.test", "C"), "1.3"))
    ))
    assert(repo.packages.size === 3)
  }

}
