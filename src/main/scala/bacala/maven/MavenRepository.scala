package bacala.maven

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.concurrent.TrieMap

import bacala.core._
import bacala.util.Worker
import Scope._

/** Constructs the repository from initial constraints
  */
class MavenRepository(initial: MavenPomFile)(parser: Worker[MavenPackage, MavenPomFile], metaParser: Worker[MavenArtifact, Iterable[String]]) extends Repository {
  type PackageT = MavenPackage
  type DependenciesT = Set[Set[PackageT]]

  private val dependencies = new TrieMap[PackageT, DependenciesT]
  private val conflictSet = new TrieMap[(PackageT, PackageT), Unit]
  private val artifactsMap = new TrieMap[MavenArtifact, Set[PackageT]]

  // the root package
  def root = initial.pkg

  def construct(scope: Scope) = {
    resolve(initial, scope, Set(), Set())

    createConflicts
  }

  /** recursively builds the dependency closure
    */
  def resolve(pom: MavenPomFile, scope: Scope, excludes: Iterable[MavenArtifact], path: Set[MavenPackage]): Unit = {
    val MavenPomFile(pkg, depsAll, resolvers) = pom
    val deps = depsAll.filter(dep => dep.inScope(scope) && !dep.canExclude(excludes) && !dep.optional)

    // use resolvers defined in POM file
    val metaResolver = (pom.resolvers :\ metaParser) { (r, acc) =>
      acc or Workers.createMetaResolver(r.url)
    }

    val pomResolver = (pom.resolvers :\ parser) { (r, acc) =>
      acc or Workers.createPomResolver(r.url)
    }

    deps.foreach { dep =>
      metaResolver(dep.artifact).map(dep.resolve(_)) match {
        case Some(pkgs) =>
          // val set = pkgs.filter(pomResolver(_).nonEmpty).toSet
          val set = pkgs.toSet

          // update dependency set
          dependencies += pkg -> (dependencies.getOrElse(pkg, Set()) + set)

          // update conflict set
          artifactsMap += dep.artifact -> (set | artifactsMap.getOrElse(dep.artifact, Set()))

          // recursive resolve
          for {
            q <- set
            if !path.contains(q)
            pom <- pomResolver(q)
          } resolve(pom, COMPILE, dep.exclusions ++ excludes, path + pkg)

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
