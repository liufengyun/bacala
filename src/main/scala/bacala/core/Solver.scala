package bacala.core

/** Represents resolution result in the dependency graph
  */
abstract class DependencyEdge[P <: Package, D <: Dependency]
case class HealthyEdge[P <: Package, D <: Dependency](dep: D, to: P) extends DependencyEdge[P, D]
case class InfectedEdge[P <: Package, D <: Dependency](dep: D, to: P) extends DependencyEdge[P, D]
case class ConflictEdge[P <: Package, D <: Dependency](dep: D, conflicts: Set[P]) extends DependencyEdge[P, D]
case class MissingEdge[P <: Package, D <: Dependency](dep: D) extends DependencyEdge[P, D]

trait Solver {
  type PackageT <: Package
  type DependencyT <: Dependency
  type TreeT = Tree[PackageT, DependencyEdge[PackageT, DependencyT]]

  def solve: Either[Set[PackageT], TreeT]
}
