package bacala.ivy

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.concurrent.TrieMap

import bacala.core._
import bacala.util.ConsoleHelper.ColorText

/** Constructs the repository from initial constraints
  */
abstract class IvyRepository(initial: IDescriptor) extends Repository {
  type LibT = ILib
  type PackageT = IPackage
  type DependencyT = IDependency
  type DependenciesT = Set[(DependencyT, Set[PackageT])]

  case class PackageInfo(dependencies: DependenciesT, descriptor: IDescriptor,
    activeConfs: Set[String], activeArtifacts: Set[String])

  private val packagesMap = new TrieMap[PackageT, PackageInfo]
  private val librariesMap = new TrieMap[LibT, Set[PackageT]]

  def versionsResolver(lib: LibT): Option[Seq[String]]
  def descriptorResolver(pkg: PackageT): Option[IDescriptor]

  // the root package
  def root = initial.pkg

  /** Builds the repository from initial constraints
    */
  def construct(conf: String) = {
    packagesMap.clear
    librariesMap.clear

    packagesMap.put(initial.pkg, PackageInfo(Set(), initial, Set(conf), Set()))
    resolve(initial, Set(conf), Seq(), Set())
  }

  def dump = {
    println("\n\n================  Packages   ==================".bold)
    println(packagesMap.mkString("\n\n"))

    println("\n\n================  Libraries   ==================".bold)
    println(librariesMap.mkString("\n\n"))
  }

  /** recursively builds the dependency closure
    */
  def resolve(ivy: IDescriptor, confs: Set[String], excludes: Seq[IExclude], path: Set[PackageT]): Unit = {
    val deps = ivy.filterDependencies(confs, excludes)

    packagesMap += ivy.pkg -> packagesMap(ivy.pkg).copy(
      dependencies = deps.map(_ -> Set[PackageT]()).toSet)

    deps.foreach { dep =>
      val depConfs = ivy.filterDepConfigurations(confs, dep)
      val activeExcludes = ivy.filterExcludes(confs, dep)

      versionsResolver(dep.lib).map(dep.filterVersions) match {
        case Some(pkgs) =>
          val set = pkgs.toSet

          // update dependency set
          val packageInfo = packagesMap(ivy.pkg)
          packagesMap += ivy.pkg -> packageInfo.copy(
            dependencies = packageInfo.dependencies + (dep -> set))

          // update conflict set
          librariesMap += dep.lib -> (set | librariesMap.getOrElse(dep.lib, Set()))

          // recursive resolve
          set.filter(!path.contains(_)).foreach { p =>
            print("Get descriptor for " + p)

            descriptorResolver(p) match {
              case Some(descriptor) =>
                println("   SUCCESS".green)

                val newExcludes = excludes ++ activeExcludes
                // update package info
                val packageInfo = packagesMap.getOrElseUpdate(p,
                  PackageInfo(Set(), descriptor, Set(), Set()))
                val artfs = descriptor.filterArtifacts(depConfs, newExcludes)

                // a package may be visited many times through different paths
                packagesMap += p -> packageInfo.copy(
                  activeArtifacts = packageInfo.activeArtifacts | artfs,
                  activeConfs = packageInfo.activeConfs | depConfs
                )

                if (dep.transitive)
                  resolve(descriptor, depConfs, newExcludes, path + ivy.pkg)
              case None =>
                println("   FAILED".red)
            }
          }
        case None =>
          println(s"Warning: Failed to resolve dependency $dep in ${ivy.pkg}".yellow)
          // update dependency set
          val packageInfo = packagesMap(ivy.pkg)
          packagesMap += ivy.pkg -> packageInfo.copy(
            dependencies = packageInfo.dependencies + (dep -> Set()))
      }
    }
  }

  /** Returns the packages that p depends on directly
    */
  override def apply(p: PackageT) = packagesMap(p).dependencies

  /** Returns all packages in the repository
    */
  override def packages = packagesMap.keys.filter(_ != root)

  /** Returns all primitive conflicts in the repository
    */
  override def conflicts = librariesMap.toMap

  /** Returns active artifacts associated with the given package
    */
  def artifacts(p: PackageT): Iterable[String] = packagesMap(p).activeArtifacts
}
