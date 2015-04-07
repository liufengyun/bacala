package bacala.maven

import bacala.core.{Artifact, Dependency, Package}
import bacala.maven.Scope._

/** This file defines the structure of the PomFile
  */

case class MavenArtifact(groupId: String, artifactId: String) extends Artifact {
  override def id =  groupId + ":" + artifactId

  override def toString = id
}

/**
  * Reference
  * - https://maven.apache.org/guides/introduction/introduction-to-optional-and-excludes-dependencies.html
  */
case class MavenDependency(artifact: MavenArtifact, versionRange: VersionRange, exclusions: Iterable[MavenArtifact], scope: Scope, optional: Boolean) extends Dependency {
  type PackageT = MavenPackage

  def inScope(scp: Scope) = scp match {
    case TEST => scope == COMPILE || scope == TEST
    case _ => scope == scp
  }

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

case class MavenResolver(id: String, name: String, url: String)

case class MavenPomFile(pkg: MavenPackage, deps: Iterable[MavenDependency], resolvers: Iterable[MavenResolver])
