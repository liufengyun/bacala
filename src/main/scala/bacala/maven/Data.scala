package bacala.maven

import bacala.core.{Artifact, Dependency, Package, Version}
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
case class MavenDependency(artifact: MavenArtifact, versionConstraint: String, exclusions: Iterable[MavenArtifact], scope: Scope, optional: Boolean) extends Dependency {

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
  def resolve(versions: Iterable[String]): Iterable[MavenPackage] = {
    val range = VersionRange(versionConstraint)
    val compatibleVersions = versions.filter(v => range.contains(Version(v))).toSet

    // include simple version
    val validVersions = if (Version.unapply(versionConstraint).nonEmpty)
      compatibleVersions + versionConstraint
    else compatibleVersions

    validVersions.map(v => MavenPackage(artifact, v))
  }
}

case class MavenPackage(artifact: MavenArtifact, version:String) extends Package {
  def artifactId = artifact.artifactId
  def groupId = artifact.groupId
}

case class MavenResolver(id: String, name: String, url: String)

case class MavenPomFile(pkg: MavenPackage, deps: Iterable[MavenDependency], resolvers: Iterable[MavenResolver])
