package bacala

abstract class DependencyManager {
  type Result
  type Repo <: Repository

  // call the algorithm to resolve dependency
  def resolve(repository: Repo): Result
}

abstract class Package

abstract class Repository {
  type PackageT <: Package
  type PackagesT
  type ConflictsT
  type ConstraintsT

  // return constraints of the package p
  def apply(p: PackageT)

  // all packages in the repository
  def packages: PackagesT

  // all primitive conflicts in the repository
  def conflicts: ConflictsT
}
