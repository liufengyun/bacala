package bacala.ivy

import bacala.core.{Lib, Dependency, Package, Version}

/** Defines data structure of Ivy XML file
  */

/** Represents the pair (groupId, module name), which is the unit for versioning
  */
case class ILib(groupId: String, name: String) extends Lib {
  override def id =  groupId + ":" + name

  override def toString = id
}

/** An IPackage refers to a module of a specific version
  *
  * An IPackage refers to a complex of configurations, artifacts and
  * dependencies. Which subset is effective in a specific scenario is
  * computed transitively from initial dependencies.
  *
  * IPackage is the unit of conflict. Two packages of the same library
  * but with different versions conflict.
  *
  * Two artifacts of the same IPackage never conflict.
  */
case class IPackage(lib: ILib, version: String) extends Package {
  type LibT = ILib

  def module = lib.name
  def groupId = lib.groupId
}

case class IArtifact(name: String, typ: String, ext: String,
  confs: Seq[String], url: String, packaging: String)

case class IConf(name: String, description: String, visibility: String,
  extendList: List[String], transitive: Boolean, deprecated: String)

case class IResolver(name: String, url: String, pattern: String,
  ivys: Boolean, artifacts: Boolean)

case class IExclude(lib: ILib, name: String, typ: String, ext: String,
  matcher: String, conf: Seq[String])

case class IDepArtifact(name: String, typ: String, ext: String,
  confs: Seq[String], url: String)

case class IDepChoice(name: String, typ: String, ext: String,
  matcher: String, conf: Seq[String])

case class IDependency(lib: ILib, version: String, versionConstraint: String,
  transitive: Boolean, force: Boolean, changing: Boolean, branch: String,
  artifacts: Seq[IDepArtifact], excludes: Seq[IDepChoice],
  includes: Seq[IDepChoice], confMap: Map[String, Seq[String]]) extends Dependency {
  type LibT = ILib
}

case class IOverride(lib: ILib, branch: String, version: String, matcher: String)

case class IConflict(lib: ILib, manager: String, versions: Seq[String], matcher: String)

/** Representation of Ivy file
  *
  * 1. Only represent the fields we care
  * 2. Extends between Ivy files are handled by the parser
  * 3. Inclusion of configuration files are handled by the parser
  */
case class IFile(pkg: IPackage, confs: Seq[IConf], deps: Seq[IDependency],
  resolvers: Seq[IResolver], artifacts: Seq[IArtifact], excludes: Seq[IExclude],
  overrides: Seq[IOverride], conflicts: Seq[IConflict])
