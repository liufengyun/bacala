package bacala

abstract class DependencyManager {
  type Result
  type ConstraintsT

  // call the algorithm to resolve dependency
  def resolve(initialConstraints: ConstraintsT): Result
}

abstract class Package

abstract class Repository {
  type PackageT <: Package
  type PackagesT
  type ConflictsT

  // return constraints of the package p
  def apply(p: PackageT)

  // all packages in the repository
  def packages: PackagesT

  // all primitive conflicts in the repository
  def conflicts: ConflictsT
}
