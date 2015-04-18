package bacala.core

sealed abstract class Clause extends Ordered[Clause] {
  def compare(that: Clause) = if (this == that) 0 else this.hashCode() - that.hashCode()
}

case class DependencyClause(pkg: Package, dep: Dependency) extends Clause
case class ConflictClause(artifact: Artifact, pkgs: Iterable[Package]) extends Clause

/** represents a tree structure
  */
case class Tree[T](value: T, children: Seq[Tree[T]] = Nil) {
  def +(child: Tree[T]): Tree[T] = {
    Tree(value, child +: children)
  }
}

trait Solver {
  type PackageT
  def solve: Either[Set[PackageT], Seq[Clause]]
}
