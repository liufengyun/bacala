package bacala.maven

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.concurrent.TrieMap

import bacala._

case class MavenPackage(val groupId:String, artefactId: String, val version:String) extends Package {
  override def hashCode = {
    (List(groupId, artefactId, version) :\ 1) { (field, r) => r + 31*field.hashCode }
  }

  override def equals(other: Any) = other match {
    case MavenPackage(group, artefact, ver) =>
      this.groupId == group && this.artefactId == artefact && this.version == ver
    case _ => false
  }
}

class MavenRepository extends Repository {
  type PackageT = MavenPackage
  type PackagesT = Set[PackageT]
  type ConflictsT = Map[PackageT, PackageT]
  type ConstraintsT = Set[Set[PackageT]]
  type Fetcher = PackageT => ConstraintsT

  private val dependencies = new TrieMap[PackageT, ConstraintsT]

  // recursively fetch the dependency closure
  // TODO : use Future to fetch in asynchronous and in parallel
  def construct(initial: ConstraintsT, fetcher: Fetcher): Unit = {
    for {
      disjunctiveSet <- initial
      p <- disjunctiveSet
      if dependencies.putIfAbsent(p, Set()) == null // atomic
    }  {
      val constraints = fetcher(p)
      dependencies.put(p, constraints)
      construct(constraints, fetcher)
    }
  }

  override def apply(p: PackageT) = dependencies(p)

  override def packages = dependencies.keys.toSet

  override def conflicts = ???
}

