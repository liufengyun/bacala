package bacala.core

abstract class DependencyManager {
  type PackageT <: Package
  type DependencyT <: Dependency

  // call the algorithm to resolve dependency
  def resolve(initialConstraints: Iterable[DependencyT]): Iterable[PackageT]
}
