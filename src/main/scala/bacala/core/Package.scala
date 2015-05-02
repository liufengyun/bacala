package bacala.core

/** Abstract representation of a library
  *
  * A library is the unit for versioning
  */
abstract class Lib {
  def id: String // unique identifier of the library

  override def hashCode = {
    id.hashCode * 31
  }

  override def equals(other: Any) = other match {
    case lib: Lib =>
      this.id == lib.id
    case _ => false
  }
}

/** Abstract representation of a package
  *
  * Package is the unit of conflicts. Two packages of the same library but
  * with different versions conflict.
  */
abstract class Package {
  type LibT <: Lib
  def lib: LibT
  def version: String // version number

  override def toString = lib + "-" + version
}

/** Abstract representation of a dependency
  */
abstract class Dependency {
  type LibT <: Lib
  def lib: LibT // this dependency is on which library
  def versionConstraint: String // version constraint
}
