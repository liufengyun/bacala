package bacala.ivy

import bacala.core.{Lib, Dependency, Package, Version}

/** Defines data structure of Ivy XML file
  */

/** Represents the pair (groupId, module name), which is the unit for versioning
  */
case class ILib(groupId: String, name: String) extends Lib {
  override def id =  groupId + ":" + name

  def isMatch(exclude: IExclude): Boolean = {
    (exclude.lib.groupId == groupId || exclude.lib.groupId == "*") &&
    (exclude.lib.name == name || exclude.lib.name == "*")
  }

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
  confs: Seq[String], url: String) {
  def isMatch(exclude: IExclude): Boolean = {
    exclude.name == "*" || exclude.name == name
  }
}

case class IConf(name: String, description: String, visibility: String,
  extendList: Seq[String], transitive: Boolean, deprecated: String)

case class IExclude(lib: ILib, name: String, typ: String, ext: String,
  matcher: String, confs: Seq[String])

case class IDepArtifact(name: String, typ: String, ext: String,
  confs: Seq[String], url: String)

case class IInclude(name: String, typ: String, ext: String,
  matcher: String, confs: Seq[String])

case class IDependency(lib: ILib, version: String, versionConstraint: String,
  transitive: Boolean, force: Boolean, changing: Boolean,
  artifacts: Seq[IDepArtifact], excludes: Seq[IExclude],
  includes: Seq[IInclude], mapping: Map[String, Seq[String]]) extends Dependency {
  type LibT = ILib

  def isMatch(exclude: IExclude): Boolean = {
    lib.isMatch(exclude) && exclude.name == "*"
  }
}

/** Representation of Ivy file
  *
  * 1. Only represent the fields we care
  * 2. Extends between Ivy files are handled by the parser
  * 3. Inclusion of configuration files are handled by the parser
  */
case class IDescriptor(pkg: IPackage, confs: Set[IConf], deps: Seq[IDependency],
  artifacts: Set[IArtifact], excludes: Set[IExclude]) {

  /** Filters dependencies according to given effective configurations and excludes
    */
  def filterDependencies(effectiveConfs: Set[String], toExclude: Seq[IExclude]): Seq[IDependency] = {
    deps.filter { dep =>
      (effectiveConfs.contains("*") ||
        dep.mapping.contains("*") ||
        dep.mapping.keys.exists(effectiveConfs.contains)) &&
      toExclude.forall(!dep.isMatch(_))
    }
  }

  /** Filters artifacts according to given effective configurations and excludes
    */
  def filterArtifacts(effectiveConfs: Set[String], toExclude: Seq[IExclude]): Set[String] = {
    artifacts.filter { artifact =>
      (effectiveConfs.contains("*") ||
        artifact.confs.contains("*") ||
        artifact.confs.exists(effectiveConfs.contains)) &&
      toExclude.forall(exclude => !pkg.lib.isMatch(exclude) || !artifact.isMatch(exclude))
    } map(_.name)
  }

  /** Filters excludes which are effective in the transitive dependency *dep*
    */
  def filterExcludes(effectiveConfs: Set[String], dep: IDependency): Seq[IExclude] = {
    (dep.excludes ++ excludes) filter { exclude =>
      effectiveConfs.contains("*") ||
      exclude.confs.contains("*") ||
      exclude.confs.exists(effectiveConfs.contains)
    }
  }

  /** Filters active configurations in the dependency module
    */
  def filterDepConfigurations(effectiveConfs: Set[String], dep: IDependency): Set[String] = {
    (dep.mapping :\ Set[String]()) { case ((from, tos), acc) =>
      if (effectiveConfs.contains("*") ||
        effectiveConfs.contains(from) ||
        from == "*")
        acc ++ tos
      else
        acc
    }
  }
}
