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
abstract class MavenRepository(initial: MDescriptor) extends Repository {
  type LibT = MLib
  type PackageT = MPackage
  type DependencyT = MDependency
  type DependenciesT = Set[(DependencyT, Set[PackageT])]

  // resolvers
  def makeResolver(resolvers: Iterable[MResolver]):
      (MPackage => Option[MDescriptor], MLib => Option[Iterable[String]])

  private val dependencies = new TrieMap[PackageT, DependenciesT]
  private val libraries = new TrieMap[LibT, Set[PackageT]]

  // the root package
  def root = initial.pkg

  /** Builds the repository from initial constraints
    */
  def construct(scope: Scope) = {
    dependencies.clear
    libraries.clear
    resolve(initial, scope, Set(), Set())
  }

  /** recursively builds the dependency closure
    */
  def resolve(pom: MDescriptor, scope: Scope, excludes: Iterable[LibT], path: Set[PackageT]): Unit = {
    val MDescriptor(pkg, _, resolvers) = pom
    val deps = pom.filterDependencies(scope, excludes)

    // the resolvers will be added to the default resolver
    val (pomResolver, metaResolver) = makeResolver(resolvers)

    deps.foreach { dep =>
      metaResolver(dep.lib).map(vers => dep.resolve(vers).filter(p => pomResolver(p).nonEmpty)) match {
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
          libraries += dep.lib -> (set | libraries.getOrElse(dep.lib, Set()))

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
  override def conflicts = libraries.toMap
}
