package bacala.core

/**
  * abstract representation of a package
  */
abstract class Package {
  type DependencyT <: Dependency

  def id: String // unique identifier of the library
  def version: String // version number

  override def hashCode = {
    (id.hashCode + version.hashCode) * 31
  }

  override def equals(other: Any) = other match {
    case that: Package =>
      this.id == that.id &&  this.version == that.version
    case _ => false
  }

  override def toString = id + "-" + version
}

/**
  * Abstract representation of a dependency
  */
abstract class Dependency {
  type PackageT <: Package

  // packages compatible with this dependency
  def resolve: Option[Iterable[PackageT]]
}
