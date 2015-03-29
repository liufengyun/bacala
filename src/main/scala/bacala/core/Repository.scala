package bacala.core

/**
  * Abstract representation of the dependency closure of a group of packages
  */
abstract class Repository {
  type PackageT <: Package
  type DependencyT <: Dependency
  type ConflictT = (PackageT, PackageT)

  // return constraints closure of the package p
  def apply(p: PackageT): Iterable[Iterable[PackageT]]

  // return direct dependencies of a package
  def dependencies(p: PackageT): Iterable[DependencyT]

  // all packages in the repository
  def packages: Iterable[PackageT]

  // all primitive conflicts in the repository
  def conflicts: Iterable[ConflictT]
}
