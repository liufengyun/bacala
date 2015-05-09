package bacala.ivy

import bacala.core.{Lib, Dependency, Package, VersionSelector}

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

  // packages compatible with this dependency
  def filterVersions(versions: Iterable[String]): Iterable[IPackage] = {
    println("Getted version for " + lib + "(" + versions.mkString(",")  + ")")
    VersionSelector(versionConstraint).choose(versions).map(IPackage(lib, _))
  }
}

/** Representation of Ivy file
  *
  * 1. Only represent the fields we care
  * 2. Extends between Ivy files are handled by the parser
  * 3. Inclusion of configuration files are handled by the parser
  */
case class IDescriptor(pkg: IPackage, confs: Map[String, IConf], deps: Seq[IDependency],
  artifacts: Map[String, IArtifact], excludes: Set[IExclude]) {

  /** Returns a set of configurations that are extended by the given configurations
    *
    * The returned set is a super set of the original set.
    */
  def getExtendedConfigurations(confSet: Set[String]) = {
    def allExtends(conf: String): Set[String] = {
      val list = confs(conf).extendList.toSet

      list ++ (list :\ Set[String]()) { (elem, acc) =>
        acc ++ allExtends(elem)
      }
    }

    (confSet :\ confSet) { (conf, acc) =>
      if (confs.contains(conf)) acc ++ allExtends(conf)
      else acc
    }
  }

  /** Filters dependencies according to given effective configurations and excludes
    */
  def filterDependencies(confSet: Set[String], toExclude: Seq[IExclude]): Seq[IDependency] = {
    val effectiveConfs = getExtendedConfigurations(confSet)
    deps.filter { dep =>
      (effectiveConfs.contains("*") ||
        dep.mapping.contains("*") ||
        dep.mapping.keys.exists(effectiveConfs.contains)) &&
      toExclude.forall(!dep.isMatch(_))
    }
  }

  /** Filters artifacts according to given effective configurations and excludes
    */
  def filterArtifacts(confSet: Set[String], toExclude: Seq[IExclude]): Set[String] = {
    val effectiveConfs = getExtendedConfigurations(confSet)
    (artifacts.filter { case (name, artifact) =>
      (effectiveConfs.contains("*") ||
        artifact.confs.contains("*") ||
        artifact.confs.exists(effectiveConfs.contains)) &&
      !toExclude.exists(exclude => pkg.lib.isMatch(exclude) && artifact.isMatch(exclude))
    }).map(_._2.name).toSet
  }

  /** Filters excludes which are effective in the transitive dependency *dep*
    */
  def filterExcludes(confSet: Set[String], dep: IDependency): Seq[IExclude] = {
    val effectiveConfs = getExtendedConfigurations(confSet)
    (dep.excludes ++ excludes) filter { exclude =>
      effectiveConfs.contains("*") ||
      exclude.confs.contains("*") ||
      exclude.confs.exists(effectiveConfs.contains)
    }
  }

  /** Filters active configurations in the dependency module
    */
  def filterDepConfigurations(confSet: Set[String], dep: IDependency): Set[String] = {
    val effectiveConfs = getExtendedConfigurations(confSet)
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
