package bacala.core

/** Represents a tree structure
  */
abstract class Tree[+T, +E] {
  def value: T
  def children: List[(E, Tree[T, E])]
  def +[B >: T, D >: E](edge: D, child: Tree[B, D]): Tree[B, D]
}

/** Represents non-empty tree
  */
case class Node[+T, +E](value: T, children: List[(E, Tree[T, E])] = List[(E, Tree[T, E])]()) extends Tree[T, E] {
  override def +[B >: T, D >: E](edge: D, child: Tree[B, D]): Tree[B, D] = {
    Node(value, (edge -> child)::children)
  }
}

/** Represents empty leaf node -- it's useful to represent missing dependency or conflict
  */
case object Leaf extends Tree[Nothing, Nothing] {
  override def value = throw new Error("Leaf node")
  override def children = throw new Error("Leaf node")
  override def +[B >: Nothing, D >: Nothing](edge: D, child: Tree[B, D]) = throw new Error("Leaf node")
}

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
