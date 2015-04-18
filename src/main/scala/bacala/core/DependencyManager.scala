package bacala.core

abstract class DependencyManager {
  type PackageT <: Package

  // call the algorithm to resolve dependency
  def resolve: Either[Iterable[PackageT], Set[String]]
}
