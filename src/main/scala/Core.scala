package bacala.core

abstract class DependencyManager {
  type Result
  type Repo <: Repository

  def resolve(repository: Repo): Result
}

abstract class Package

abstract class Repository {
  type PackageT <: Package
  type Conflict
  type Constraints

  // recursively construct the repository from initial constraints to form dependency closure
  def construct(initial: Constraints, fetcher: PackageT => Constraints)

  // return a conjunctive set of disjunctive sets of packages
  def apply(p: PackageT)

  // all packages in the repository
  def packages: Set[PackageT]

  // all basic conflicts in the repository
  def conflicts: Set[Conflict]
}
