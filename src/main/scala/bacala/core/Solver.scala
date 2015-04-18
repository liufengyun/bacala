package bacala.core

trait Solver {
  type PackageT
  def solve: Either[Set[PackageT], Set[String]]
}
