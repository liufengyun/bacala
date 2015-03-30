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
  def resolve(versions: Iterable[Version]): Iterable[PackageT] = {
    val compatibleVersions = versions.filter(v => versionRange.contains(v))
    compatibleVersions.map(v => MavenPackage(artifact, v.toString)).toSet
  }

  // dependency graph building
  def resolve(p: MavenPackage, scope: Scope, excludes: Iterable[MavenArtifact]): Map[MavenPackage, Set[Set[MavenPackage]]] = ???

}

case class MavenPackage(artifact: MavenArtifact, version:String) extends Package {
  type DependencyT = MavenDependency

  def artifactId = artifact.artifactId
  def groupId = artifact.groupId

  def resolve(dependencies: Iterable[MavenDependency], excludes: Iterable[MavenArtifact]): Option[Set[Set[MavenPackage]]] = ???
}

class MavenRepository(initialDependencies: Iterable[MavenDependency])(parser: MavenPackage => Option[Iterable[MavenDependency]], metaParser: MavenArtifact => Option[Iterable[Version]]) extends Repository {
  type PackageT = MavenPackage
  type ConstraintsT = Set[Set[PackageT]]
  type DependencyT = MavenDependency

  private val dependencies = new TrieMap[PackageT, ConstraintsT]
  private val directDependencies = new TrieMap[PackageT, Iterable[MavenDependency]]
  private val failureSet = new TrieMap[PackageT, Unit]
  private val conflictSet = new TrieMap[(PackageT, PackageT), Unit]

  implicit val fetcher = MavenFetcher

  def construct(scope: Scope) = {
    for {
      dep <- initialDependencies
      if dep.scope == scope
      set <- metaParser(dep.artifact).map(dep.resolve(_))
      p <- set
    } resolve(p, scope, dep.exclusions)

    createConflicts
  }

  // recursively fetch the dependency closure
  // FIX: A ignores d in someplace, but A requires d in another place
  def resolve(p: MavenPackage, scope: Scope, excludes: Iterable[MavenArtifact]): Unit = {
    if (!failureSet.contains(p) && !dependencies.contains(p))
      parser(p) map { deps =>
        deps.filter(dep => dep.inScope(scope) && !dep.canExclude(excludes))
      } match {
        case Some(deps) =>
          directDependencies += p -> deps

          val sets = (for {
            dep <- deps
            set <- metaParser(dep.artifact).map(dep.resolve(_))
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
