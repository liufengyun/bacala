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

  def initialize(initial: ConstraintsT, fetcher: Fetcher): Unit = {
    construct(initial, fetcher)
    createConflicts
  }

  // recursively fetch the dependency closure
  // TODO : use Future to fetch in asynchronous and in parallel
  def construct(initial: ConstraintsT, fetcher: Fetcher): Unit = {
    for {
      disjunctiveSet <- initial
      p <- disjunctiveSet
      if !failureSet.contains(p)
      if dependencies.putIfAbsent(p, Set()) == None // atomic
    }
      fetcher(p) match {
        case Some(constraints) =>
          dependencies += p -> constraints
          construct(constraints, fetcher)
        case None =>
          dependencies -= p
          failureSet += p -> ()
      }
  }

  // Intialize conflicts structure from repository data
  // TODO: reduce symmetric duplicates, (p, q) and (q, p)
  private def createConflicts = {
    val pkgs = this.packages

    val conflicts = for {
      p <- pkgs
      q <- pkgs - p
      if inConflict(p, q)
    } yield (p, q) -> ()

    conflictSet ++= conflicts
  }

  // Packages with the same artefactId but different versions are in conflict
  def inConflict(p: MavenPackage, q: MavenPackage): Boolean = {
    p.groupId == q.groupId && p.artifactId == q.artifactId
  }

  override def apply(p: PackageT) = dependencies(p)

  override def packages = dependencies.keys.toSet

  override def conflicts = conflictSet.keys.toSet
}
