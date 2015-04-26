package bacala.test.alg

import bacala.core._

case class Art(val id: String) extends Artifact
case class Pack(val artifact: Art, val version: String) extends Package {
  type ArtifactT = Art
}
case class Dep(val artifact: Art, val versionConstraint: String) extends Dependency {
  type ArtifactT = Art
}

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
  type ArtifactT = Art
  type PackageT = Pack
  type DependencyT = Dep

  def map: Map[PackageT, Set[(Dep, Set[PackageT])]]

  override def root = Pack(Root, "2.4.2")

  override def apply(p: PackageT) = map(p)

  override def packages = map.keys.filter(_ != root)
}
