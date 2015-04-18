package bacala.core

trait Solver {
  type PackageT
  def solve: Option[Iterable[PackageT]]
}
