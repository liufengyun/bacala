package bacala.maven

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.concurrent.TrieMap

import bacala.core._
import bacala.util.Worker
import bacala.util.ConsoleHelper.ColorText
import Scope._

/** Constructs the repository from initial constraints
  */
abstract class MavenRepository(initial: MavenPomData) extends Repository {
  type ArtifactT = MavenArtifact
  type PackageT = MavenPackage
  type DependencyT = MavenDependency
  type DependenciesT = Set[(DependencyT, Set[PackageT])]

  // resolvers
  def makePomResolver(resolvers: Iterable[MavenResolver]): MavenPackage => Option[MavenPomData]
  def makeMetaResolver(resolvers: Iterable[MavenResolver]): MavenArtifact => Option[Iterable[String]]

  private val dependencies = new TrieMap[PackageT, DependenciesT]
  private val artifactsMap = new TrieMap[ArtifactT, Set[PackageT]]

  // the root package
  def root = initial.pkg

  /** Builds the repository from initial constraints
    */
  def construct(scope: Scope) = {
    dependencies.clear
    artifactsMap.clear
    resolve(initial, scope, Set(), Set())
  }

  /** recursively builds the dependency closure
    */
  def resolve(pom: MavenPomData, scope: Scope, excludes: Iterable[ArtifactT], path: Set[PackageT]): Unit = {
    val MavenPomData(pkg, depsAll, resolvers) = pom
    val deps = depsAll.filter(dep => dep.inScope(scope) && !dep.canExclude(excludes) && !dep.optional)

    // the resolvers will be added to the default resolver
    val metaResolver = makeMetaResolver(resolvers)
    val pomResolver = makePomResolver(resolvers)

    deps.foreach { dep =>
      metaResolver(dep.artifact).map(vers => dep.resolve(vers).filter(p => pomResolver(p).nonEmpty)) match {
        case Some(pkgs) =>
          val set = pkgs.toSet

          // set can't be empty for root
          if (set.isEmpty && pkg == root) {
            println(s"Fatal Error: can't find match version for root dependency $dep".red)
            System.exit(1)
          }

          // update dependency set
          dependencies += pkg -> ((dependencies.getOrElse(pkg, Set()) + (dep -> set)))

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
                println(s"Error: failed to download POM file for $p".red)
            }
          }
        case None =>
          if (pkg == root) {
            println(s"Fatal Error: Can't resolve root dependency $dep".red)
            System.exit(1)
          } else {
            println(s"Warning: Failed to resolve dependency $dep in $pkg".yellow)
            // update dependency set
            dependencies += pkg -> ((dependencies.getOrElse(pkg, Set()) + (dep -> Set())))
          }
      }
    }
  }

  /** Returns the packages that p depends on directly
    */
  override def apply(p: PackageT) = dependencies(p)

  /** Returns all packages in the repository
    */
  override def packages = dependencies.keys.filter(_ != root)

  /** Returns all primitive conflicts in the repository
    */
  override def conflicts = artifactsMap.toMap
}
