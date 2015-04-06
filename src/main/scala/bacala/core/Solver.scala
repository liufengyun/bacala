package bacala.core

trait Solver {
  def solve[T <: Repository](repository: T): Option[Iterable[repository.PackageT]]
}
