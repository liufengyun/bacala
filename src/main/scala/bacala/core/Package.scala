package bacala.core

/** Abstract representation of a library
  */

abstract class Artifact {
  def id: String // unique identifier of the library

  override def hashCode = {
    id.hashCode * 31
  }

  override def equals(other: Any) = other match {
    case artifact: Artifact =>
      this.id == artifact.id
    case _ => false
  }
}

/** Abstract representation of a package
  */
abstract class Package {
  type ArtifactT <: Artifact
  def artifact: ArtifactT
  def version: String // version number

  override def toString = artifact + "-" + version
}

/** Abstract representation of a dependency
  */
abstract class Dependency {
  type ArtifactT <: Artifact
  def artifact: ArtifactT // this dependency is on which artifact
  def versionConstraint: String // version constraint
}
