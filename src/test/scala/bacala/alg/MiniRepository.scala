package bacala.test.alg

import bacala.core._

case class Art(val id: String) extends Artifact
case class Pack(val artifact: Art, val version: String) extends Package
case class Dep(val artifact: Art, val versionConstraint: String) extends Dependency

object Root extends Art("Root")
object A extends Art("A")
object B extends Art("B")
object C extends Art("C")
object D extends Art("D")
object E extends Art("E")
object F extends Art("F")
object G extends Art("G")
object H extends Art("H")
object I extends Art("I")
object J extends Art("J")
object K extends Art("K")

abstract class MiniRepository extends Repository {
  type PackageT = Pack

  def map: Map[PackageT, Set[(Dep, Set[PackageT])]]

  override def root = Pack(Root, "2.4.2")

  override def apply(p: PackageT) = map(p)

  override def packages = map.keys.filter(_ != root)

  override def conflicts: Set[(Art, Iterable[Pack])]
}
