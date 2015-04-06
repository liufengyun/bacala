package bacala.maven

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.concurrent.TrieMap

import bacala.core._
import Scope._

case class MavenArtifact(groupId: String, artifactId: String) extends Artifact {
  override def id =  groupId + ":" + artifactId

  override def toString = id
}

/**
  * Reference
  * - https://maven.apache.org/guides/introduction/introduction-to-optional-and-excludes-dependencies.html
  */
case class MavenDependency(artifact: MavenArtifact, versionRange: VersionRange, exclusions: Iterable[MavenArtifact], scope: Scope, optional:Boolean) extends Dependency {
  type PackageT = MavenPackage

  // whether current dependency can be excluded
  def canExclude(exclusions: Iterable[MavenArtifact]) = exclusions.exists { exclude =>
    (exclude == this.artifact) ||
    (exclude.groupId == this.artifact.groupId && exclude.artifactId == "*") ||
    (exclude.groupId == "*" && exclude.artifactId == "*")
  }

  // packages compatible with this dependency
  def resolve(versions: Iterable[String]): Iterable[PackageT] = {
    val compatibleVersions = versions.filter(v => versionRange.contains(Version(v)))
    compatibleVersions.map(v => MavenPackage(artifact, v)).toSet
  }
}

case class MavenPackage(artifact: MavenArtifact, version:String) extends Package {
  def artifactId = artifact.artifactId
  def groupId = artifact.groupId
}

class MavenRepository(initialDependencies: Iterable[MavenDependency])(parser: MavenPackage => Option[Iterable[MavenDependency]], metaParser: MavenArtifact => Option[Iterable[String]]) extends Repository {
  type PackageT = MavenPackage
  type DependenciesT = Set[Set[PackageT]]
  type DependencyT = MavenDependency

  private val dependencies = new TrieMap[PackageT, DependenciesT]
  private val conflictSet = new TrieMap[(PackageT, PackageT), Unit]
  private val artifactsMap = new TrieMap[MavenArtifact, Set[PackageT]]

  def construct(scope: Scope) = {
    for {
      dep <- initialDependencies
      if dep.scope == scope
      set <- metaParser(dep.artifact).map(dep.resolve(_))
      p <- set
    } resolve(p, dep.exclusions, Set())

    createConflicts
  }

  /** recursively builds the dependency closure
    */
  def resolve(p: MavenPackage, excludes: Iterable[MavenArtifact], path: Set[MavenPackage]): Unit = {
    parser(p) map { deps =>
      deps.filter(dep => dep.scope == COMPILE && !dep.canExclude(excludes))
    } match {
      case Some(deps) =>
        // resolve direct dependency
        val sets = (for {
          dep <- deps
          art = dep.artifact
          set <- metaParser(art).map(dep.resolve(_))
        } yield set.filter(parser(_).nonEmpty).toSet).toSet

        // excludes in one path may be included in another path
        dependencies += p -> (sets | dependencies.getOrElse(p, Set()))

        val map = sets zip deps

        // update conflict set
        map.foreach { case (set, dep) =>
          artifactsMap += dep.artifact -> (set | artifactsMap.getOrElse(dep.artifact, Set()))
        }

        // recursive resolve
        for {
          (set, dep) <- map
          q <- set
          if !path.contains(q)
        } resolve(q, dep.exclusions ++ excludes, path + p)
      case None =>
    }
  }

  /** Intialize conflicts structure from repository data
    */
  private def createConflicts = {
    val pkgs = this.packages

    val conflicts = for {
      (_, pkgs) <- artifactsMap
      p <- pkgs
      q <- pkgs
      if p != q
      if !conflictSet.contains((p, q))
      if !conflictSet.contains((q, p))
    } conflictSet += (p, q) -> ()
  }

  // Packages with the same artefactId but different versions are in conflict
  def inConflict(p: MavenPackage, q: MavenPackage): Boolean = {
    p.artifact == q.artifact && p.version != q.version
  }

  /** Returns the packages that p depends on directly
    */
  override def apply(p: PackageT) = dependencies(p)

  /** Returns all packages in the repository
    */
  override def packages = dependencies.keys

  /** Returns all primitive conflicts in the repository
    */
  override def conflicts = conflictSet.keys
}
