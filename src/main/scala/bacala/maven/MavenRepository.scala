package bacala.maven

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.concurrent.TrieMap

import bacala.core._
import bacala.util.Worker
import Scope._

/** Constructs the repository from initial constraints
  */
abstract class MavenRepository(initial: MavenPomFile) extends Repository {
  type PackageT = MavenPackage
  type DependenciesT = Set[Set[PackageT]]

  // fetchers and resolvers
  def initPomFetcher: Worker[MavenPackage, String]
  def initMetaFetcher: Worker[MavenArtifact, String]
  def initPomResolver: Worker[MavenPackage, MavenPomFile]
  def initMetaResolver: Worker[MavenArtifact, Iterable[String]]
  def makePomFetcher(url: String): Worker[MavenPackage, String]
  def makeMetaFetcher(url: String): Worker[MavenArtifact, String]
  def makePomResolver(fetcher: Worker[MavenPackage, String]): Worker[MavenPackage, MavenPomFile]
  def makeMetaResolver(fetcher: Worker[MavenArtifact, String]): Worker[MavenArtifact, Iterable[String]]

  private val dependencies = new TrieMap[PackageT, DependenciesT]
  private val conflictSet = new TrieMap[(PackageT, PackageT), Unit]
  private val artifactsMap = new TrieMap[MavenArtifact, Set[PackageT]]

  // the root package
  def root = initial.pkg

  def construct(scope: Scope) = {
    resolve(initial, scope, Set(), Set())

    createConflicts

    println("*****all packages in repository******")
    println(dependencies.mkString("\n"))
    println("*****all conflicts in repository******")
    println(artifactsMap.mkString("\n"))
  }

  /** recursively builds the dependency closure
    */
  def resolve(pom: MavenPomFile, scope: Scope, excludes: Iterable[MavenArtifact], path: Set[MavenPackage]): Unit = {
    val MavenPomFile(pkg, depsAll, resolvers) = pom
    val deps = depsAll.filter(dep => dep.inScope(scope) && !dep.canExclude(excludes) && !dep.optional)

    // use resolvers defined in POM file
    val metaFetcher = (pom.resolvers :\ initMetaFetcher) { (r, acc) =>
      acc or makeMetaFetcher(r.url)
    }

    val pomFetcher = (pom.resolvers :\ initPomFetcher) { (r, acc) =>
      acc or makePomFetcher(r.url)
    }

    // chain with initMetaResolver to enable caching
    val metaResolver = initMetaResolver or makeMetaResolver(metaFetcher)
    val pomResolver = initPomResolver or makePomResolver(pomFetcher)

    deps.foreach { dep =>
      metaResolver(dep.artifact).map(dep.resolve(_)) match {
        case Some(pkgs) =>
          val set = pkgs.toSet

          // set can't be empty for root
          if (set.isEmpty && pkg == root) {
            println("Fatal Error: can't find match version for root dependency " + dep)
            System.exit(1)
          }

          // update dependency set
          dependencies += pkg -> (dependencies.getOrElse(pkg, Set()) + set)

          // update conflict set
          artifactsMap += dep.artifact -> (set | artifactsMap.getOrElse(dep.artifact, Set()))

          // recursive resolve
          set.filter(!path.contains(_)).foreach { p =>
            // important: initialize dependency
            dependencies.getOrElseUpdate(p, Set())

            pomResolver(p) match {
              case Some(pom) =>
                resolve(pom, COMPILE, dep.exclusions ++ excludes, path + pkg)
              case None =>
                println(s"Error: failed to download POM file for $p")
            }
          }
        case None =>
          if (pkg == root) {
            println("Fatal Error: Can't resolve root dependency " + dep)
            System.exit(1)
          } else {
            println(s"Warning: Failed to resolve dependency $dep in $pkg")
          }
      }
    }
  }

  /** Intialize conflicts structure from repository data
    */
  private def createConflicts = {
    val pkgs = this.packages

    val conflicts = for {
      (_, pkgs) <- artifactsMap
      p <- pkgs
      q <- pkgs
      if p != q
      if !conflictSet.contains((p, q))
      if !conflictSet.contains((q, p))
    } conflictSet += (p, q) -> ()
  }

  // Packages with the same artefactId but different versions are in conflict
  def inConflict(p: MavenPackage, q: MavenPackage): Boolean = {
    p.artifact == q.artifact && p.version != q.version
  }

  /** Returns the packages that p depends on directly
    */
  override def apply(p: PackageT) = dependencies(p)

  /** Returns all packages in the repository
    */
  override def packages = dependencies.keys

  /** Returns all primitive conflicts in the repository
    */
  override def conflicts = conflictSet.keys
}
