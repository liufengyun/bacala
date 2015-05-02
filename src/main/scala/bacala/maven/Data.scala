package bacala.maven

import bacala.core._
import bacala.maven.Scope._

/** This file defines the structure of the PomFile
  */

case class MLib(groupId: String, artifactId: String) extends Lib {
  override def id =  groupId + ":" + artifactId

  override def toString = id
}

case class MPackage(lib: MLib, version:String) extends Package {
  type LibT = MLib

  def artifactId = lib.artifactId
  def groupId = lib.groupId
}

/**
  * Reference
  * - https://maven.apache.org/guides/introduction/introduction-to-optional-and-excludes-dependencies.html
  */
case class MDependency(lib: MLib, versionConstraint: String, exclusions: Iterable[MLib], scope: Scope, optional: Boolean) extends Dependency {
  type LibT = MLib

  def inScope(scp: Scope) = scp match {
    case TEST => scope == COMPILE || scope == TEST
    case _ => scope == scp
  }

  // whether current dependency can be excluded
  def canExclude(exclusions: Iterable[MLib]) = exclusions.exists { exclude =>
    (exclude == this.lib) ||
    (exclude.groupId == this.lib.groupId && exclude.artifactId == "*") ||
    (exclude.groupId == "*" && exclude.artifactId == "*")
  }

  // packages compatible with this dependency
  def resolve(versions: Iterable[String]): Iterable[MPackage] = {
    val range = VersionRange(versionConstraint)
    val compatibleVersions = versions.filter(v => range.contains(Version(v))).toSet

    // include simple version
    val validVersions = if (Version.unapply(versionConstraint).nonEmpty)
      compatibleVersions + versionConstraint
    else compatibleVersions

    validVersions.map(v => MPackage(lib, v))
  }

  override def toString = {
    lib.toString + "(" + versionConstraint + ")"
  }
}

case class MResolver(id: String, name: String, url: String)

case class MFile(pkg: MPackage, deps: Iterable[MDependency], resolvers: Iterable[MResolver])
