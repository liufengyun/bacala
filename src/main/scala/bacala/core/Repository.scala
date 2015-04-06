package bacala.core

/**
  * Abstract representation of the dependency closure of a group of packages
  */
abstract class Repository {
  type PackageT <: Package
  type ConflictT = (PackageT, PackageT)

  /** Returns initial constraints for the repository
    */
  def seeds: Iterable[Iterable[PackageT]]

  /** Returns the packages that p depends on directly
    */
  def apply(p: PackageT): Iterable[Iterable[PackageT]]

  /** Returns all packages in the repository
    */
  def packages: Iterable[PackageT]

  /** Returns all primitive conflicts in the repository
    */
  def conflicts: Iterable[ConflictT]
}
