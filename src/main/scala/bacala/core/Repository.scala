package bacala.core

/**
  * Abstract representation of the dependency closure of a group of packages
  */
abstract class Repository { outer =>
  type LibT   <: Lib
  type PackageT    <: Package { type LibT = outer.LibT }
  type DependencyT <: Dependency { type LibT = outer.LibT }

  /** Returns the root package for the repository
    */
  def root: PackageT

  /** Returns the packages that p depends on directly
    */
  def apply(p: PackageT): Iterable[(DependencyT, Iterable[PackageT])]

  /** Returns all packages in the repository, except the root
    */
  def packages: Iterable[PackageT]

  /** Returns all primitive conflicts in the repository
    */
  def conflicts: Map[LibT, Set[PackageT]]
}
