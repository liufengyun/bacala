package bacala.maven

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.concurrent.TrieMap

import bacala.core._
import Scope._

case class MavenArtifact(groupId: String, artifactId: String) extends Artifact {
  override def id =  groupId + ":" + artifactId
}

/**
  * Reference
  * - https://maven.apache.org/guides/introduction/introduction-to-optional-and-excludes-dependencies.html
  */
case class MavenDependency(artifact: MavenArtifact, versionRange: VersionRange, exclusions: Iterable[MavenArtifact], scope: Scope, optional:Boolean) extends Dependency {
  type PackageT = MavenPackage

  // whether current dependency is in the given scope
  def inScope(scope: Scope) = scope == this.scope

  // whether current dependency can be excluded
  def canExclude(exclusions: Iterable[MavenArtifact]) = exclusions.exists { exclude =>
    (exclude == this.artifact) ||
    (exclude.groupId == this.artifact.groupId && exclude.artifactId == "*") ||
    (exclude.groupId == "*" && exclude.artifactId == "*")
  }

  // packages compatible with this dependency
  def resolve: Option[Iterable[PackageT]] = {
    MetaFile(artifact) map { versions =>
      val compatibleVersions = versions.filter(s => versionRange.contains(Version(s)))
      compatibleVersions.map(v => MavenPackage(artifact, v)).toSet
    }
  }
}

case class MavenPackage(artifact: MavenArtifact, version:String) extends Package {
  type DependencyT = MavenDependency

  def artifactId = artifact.artifactId
  def groupId = artifact.groupId
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
    } resolve(p, scope, dep.exclusions)

    createConflicts
  }

  // recursively fetch the dependency closure
  // FIX: A ignores d in someplace, but A requires d in another place
  def resolve(p: MavenPackage, scope: Scope, excludes: Iterable[MavenArtifact]): Unit = {
    if (!failureSet.contains(p) && directDependencies.putIfAbsent(p, Set()) == None) // atomic
      MavenPomParser(p) map { deps => deps.filter(_.inScope(scope)).filter(dep =>
        !dep.canExclude(excludes)
      )} match {
        case Some(deps) =>
          directDependencies += p -> deps

          val sets = (for {
            dep <- deps
            set <- dep.resolve
          } yield set.toSet).toSet

          dependencies += p -> sets

          for ((set, dep) <- sets zip deps; q <- set) resolve(q, scope, dep.exclusions ++ excludes)
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
    p.artifact == q.artifact && p.version != q.version
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
