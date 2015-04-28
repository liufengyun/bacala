package bacala.maven

import bacala.core.{JLib, Dependency, JPackage, Version}
import bacala.maven.Scope._

/** This file defines the structure of the PomFile
  */


/**
  * Reference
  * - https://maven.apache.org/guides/introduction/introduction-to-optional-and-excludes-dependencies.html
  */
case class MavenDependency(lib: JLib, versionConstraint: String, exclusions: Iterable[JLib], scope: Scope, optional: Boolean) extends Dependency {
  type LibT = JLib

  def inScope(scp: Scope) = scp match {
    case TEST => scope == COMPILE || scope == TEST
    case _ => scope == scp
  }

  // whether current dependency can be excluded
  def canExclude(exclusions: Iterable[JLib]) = exclusions.exists { exclude =>
    (exclude == this.lib) ||
    (exclude.groupId == this.lib.groupId && exclude.artifactId == "*") ||
    (exclude.groupId == "*" && exclude.artifactId == "*")
  }

  // packages compatible with this dependency
  def resolve(versions: Iterable[String]): Iterable[JPackage] = {
    val range = VersionRange(versionConstraint)
    val compatibleVersions = versions.filter(v => range.contains(Version(v))).toSet

    // include simple version
    val validVersions = if (Version.unapply(versionConstraint).nonEmpty)
      compatibleVersions + versionConstraint
    else compatibleVersions

    validVersions.map(v => JPackage(lib, v))
  }

  override def toString = {
    lib.toString + "(" + versionConstraint + ")"
  }
}

case class MavenResolver(id: String, name: String, url: String)

case class MavenPomData(pkg: JPackage, deps: Iterable[MavenDependency], resolvers: Iterable[MavenResolver])
