package bacala.maven

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.concurrent.TrieMap

import bacala._

case class MavenPackage(groupId:String, artifactId:String, version:String) extends Package {
  override def hashCode = {
    (List(groupId, artifactId, version) :\ 1) { (field, r) => r + 31*field.hashCode }
  }

  override def equals(other: Any) = other match {
    case MavenPackage(group, artifact, ver) =>
      this.groupId == group && this.artifactId == artifact && this.version == ver
    case _ => false
  }

  override def toString = groupId + ":" + artifactId + "-" + version
}

class MavenRepository extends Repository {
  type PackageT = MavenPackage
  type PackagesT = Set[PackageT]
  type ConflictsT = Set[(PackageT, PackageT)]
  type ConstraintsT = Set[Set[PackageT]]
  type Fetcher = PackageT => Option[ConstraintsT]

  private val dependencies = new TrieMap[PackageT, ConstraintsT]
  private val failureSet = new TrieMap[PackageT, Unit]
  private val conflictSet = new TrieMap[(PackageT, PackageT), Unit]

  // recursively fetch the dependency closure
  // TODO : use Future to fetch in asynchronous and in parallel
  def construct(initial: ConstraintsT, fetcher: Fetcher): Unit = {
    // update conflict set. Packages with the same artefactId but different versions conflict
    for (disjunctiveSet <- initial) {
      // TODO: reduce symmetric duplicates, (p, q) and (q, p)
      conflictSet ++= disjunctiveSet.flatMap(p => (disjunctiveSet - p).map(q => (p, q) -> ()))
    }

    // update dependency
    for (disjunctiveSet <- initial; p <- disjunctiveSet) {
      if (!failureSet.contains(p) && dependencies.putIfAbsent(p, Set()) == None) { // atomic
        fetcher(p) match {
          case Some(constraints) =>
            dependencies += p -> constraints
            construct(constraints, fetcher)
          case None =>
            dependencies -= p
            failureSet += p -> ()
        }
      }
    }
  }

  override def apply(p: PackageT) = dependencies(p)

  override def packages = dependencies.keys.toSet

  override def conflicts = conflictSet.keys.toSet
}
