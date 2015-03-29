package bacala.maven

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.concurrent.TrieMap

import bacala.core._
import Scope._

// represent an exclusion in POM file
case class Exclusion(groupId:String, artifactId:String)

/**
  * Reference
  * - https://maven.apache.org/guides/introduction/introduction-to-optional-and-excludes-dependencies.html
  */
case class MavenDependency(groupId:String, artifactId:String, versionRange: VersionRange, exclusions: Iterable[Exclusion], scope: Scope, optional:Boolean) extends Dependency {
  type PackageT = MavenPackage

  // packages compatible with this dependency
  def resolve: Option[Iterable[PackageT]] = {
    MetaFile(groupId, artifactId) map { versions =>
      val compatibleVersions = versions.filter(s => versionRange.contains(Version(s)))
      compatibleVersions.map(v => MavenPackage(groupId, artifactId, v)).toSet
    }
  }
}

case class MavenPackage(groupId:String, artifactId:String, version:String) extends Package {
  type DependencyT = MavenDependency

  override def id = groupId + ":" + artifactId
}

class MavenRepository(initialDependencies: Iterable[MavenDependency]) extends Repository {
  type PackageT = MavenPackage
  type ConstraintsT = Set[Set[PackageT]]
  type DependencyT = MavenDependency

  private val dependencies = new TrieMap[PackageT, ConstraintsT]
  private val directDependencies = new TrieMap[PackageT, Iterable[MavenDependency]]
  private val failureSet = new TrieMap[PackageT, Unit]
  private val conflictSet = new TrieMap[(PackageT, PackageT), Unit]

  def construct(scope: Scope) = {
    for {
      dep <- initialDependencies
      if dep.scope == scope
      set <- dep.resolve
      p <- set
    } resolve(p, scope)

    createConflicts
  }

  // recursively fetch the dependency closure
  def resolve(p: MavenPackage, scope: Scope): Unit = {
    if (!failureSet.contains(p) && directDependencies.putIfAbsent(p, Set()) == None) // atomic
      MavenPomParser(p) map { deps => deps.filter(_.scope == scope) } match {
        case Some(deps) =>
          directDependencies += p -> deps

          val sets = (for {
            dep <- deps
            set <- dep.resolve
          } yield set.toSet).toSet

          dependencies += p -> sets

          for (set <- sets; q <- set) resolve(q, scope)
        case None =>
          directDependencies -= p
          failureSet += p -> ()
      }
  }

  // Intialize conflicts structure from repository data
  // TODO: reduce symmetric duplicates, (p, q) and (q, p)
  private def createConflicts = {
    val pkgs = this.packages

    val conflicts = for {
      p <- pkgs
      q <- pkgs.filter(_ != p)
      if inConflict(p, q)
    } yield (p, q) -> ()

    conflictSet ++= conflicts
  }

  // Packages with the same artefactId but different versions are in conflict
  def inConflict(p: MavenPackage, q: MavenPackage): Boolean = {
    p.groupId == q.groupId && p.artifactId == q.artifactId && p.version != q.version
  }

  // closure of constraints of packages for p
  override def apply(p: PackageT) = dependencies(p)

  // return direct dependencies of a package
  override def dependencies(p: PackageT) = directDependencies(p)

  // all packages
  override def packages = dependencies.keys

  // all conflicts
  override def conflicts = conflictSet.keys
}
