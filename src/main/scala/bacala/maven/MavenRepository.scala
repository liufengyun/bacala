package bacala.maven

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{Set => MutableSet}

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
  type ConflictsT = Map[PackageT, PackageT]
  type ConstraintsT = Set[Set[PackageT]]
  type Fetcher = PackageT => Option[ConstraintsT]

  private val dependencies = new TrieMap[PackageT, ConstraintsT]
  private val failureSet = MutableSet[PackageT]()

  // recursively fetch the dependency closure
  // TODO : use Future to fetch in asynchronous and in parallel
  def construct(initial: ConstraintsT, fetcher: Fetcher): Unit = {
    for {
      disjunctiveSet <- initial
      p <- disjunctiveSet
      if !failureSet.contains(p) && dependencies.putIfAbsent(p, Set()) == None // atomic
    }  {
      fetcher(p) match {
        case Some(constraints) =>
          dependencies.put(p, constraints)
          construct(constraints, fetcher)
        case None =>
          dependencies.remove(p)
          failureSet.add(p)
      }
    }
  }

  override def apply(p: PackageT) = dependencies(p)

  override def packages = dependencies.keys.toSet

  override def conflicts = ???
}
