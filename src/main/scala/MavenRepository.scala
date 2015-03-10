package bacala.maven

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.concurrent.TrieMap

import bacala.core._

class MavenPackage(val name:String, val version:String) extends Package {
  // rewrite hashcode and equals
  override def hashCode = ???

  override def equals(other: Any) = ???
}

class MavenRepository extends Repository {
  type packaget = MavenPackage
  type Conflict = (PackageT, PackageT)
  type Constraints = Set[Set[PackageT]]

  private val dependencies = new TrieMap[PackageT, Constraints]

  // recursively fetch the dependency closure
  override def construct(initial: Constraints, fetcher: PackageT => Constraints): Unit = {
    for (set <- initial)
      for (p <- set)
        if (dependencies.putIfAbsent(p, Set()) == null) // atomic
          Future { fetcher(p) } map { constraints =>
            dependencies.put(p, constraints)
            construct(constraints, fetcher)
          }
  }

  override def apply(p: PackageT) = {
    dependencies(p)
  }

  override def packages = dependencies.keys.toSet

  override def conflicts = ???
}

