package bacala.maven


import scala.xml.XML
import scala.xml.Node
import Scope._

/**
  * A minimal, lazy and practical support of properties in POM files.
  *
  * Currently only variable properties and path properties are supported.
  *
  * A variable property is like ${someVar}, which should be defined in
  * the section /project/properties of the POM file.
  *
  * A path property is like ${project.version}. To resolve this property,
  * the resolver should start from the root of the XML tree, following the
  * path in the property, and return the text of the last matched node.
  *
  * A particularity in POM is that it has a <parent></parent> section within
  * <project></project>. The path property ${project.version} can also be
  * resolved to the path /project/parent/version if it can't be resolved
  * at the path /project/version.
  *
  * TODO: currently the implementation doesn't support the full inheritance
  * mechanism of POM file. It should be extended when there's compelling
  * need to do so.
  *
  * Reference: http://maven.apache.org/pom.html#Properties
  */
object Property {
  val propertyPat = """\s*\$\{\s*([A-Za-z0-9.]+)\s*\}\s*""".r

  def resolve(node: Node)(property: String): String = {
    // first try variable property
    val res = resolveVariable(node, property) // variable property

    if (!res.isEmpty) res else {
      val parts = property.split('.')
      resolvePath(node, parts) // path property
    }
  }

  private def resolveVariable(node: Node, property: String) = {
    (node \ "properties" \ property ).text
  }

  private def resolvePath(node: Node, path: Array[String]) = {
    val initial: Option[Node] = Some(node)
    val result = (initial /: path.tail) { (opt, part) =>
      for {
        node <- opt
        seq = node \ part
        if seq.length > 0
      } yield seq(0)
    }

    result match {
      case Some(node) => node.text
      case None =>
        ((node \ "parent") /: path.tail) { (child, part) => (child \ part)(0) }.text
    }
  }

  def unapply(s: String): Option[String] = s match {
    case propertyPat(prop) => Some(prop)
    case _ => None
  }
}

/**
  * The companion object PomFile does caching internally
  *
  * This object also breaks parent/sub-module loop
  */
object PomFile {
  var cache = Map[MavenPackage, PomFile]()

  def apply(pkg: MavenPackage) = {
    if (cache.contains(pkg)) cache(pkg) else
    MavenFetcher(pkg) match {
      case Some(spec) =>
        val pom = new PomFile(pkg, XML.loadString(spec))
        cache = cache + (pkg -> pom)
        pom
      case None =>
        println("Error: failed to get POM for " + pkg)
        null
    }
  }

  def apply(spec: String) = {
    val node = XML.loadString(spec)
    val artifactId = (node \ "artifactId").text
    // version and groupId may be inherited from /project/parent
    val groupId = Property.resolve(node)("project.groupId")
    val version = Property.resolve(node)("project.version")

    val pkg = MavenPackage(groupId, artifactId, version)
    val pom = new PomFile(pkg, node)
    cache = cache + (pkg -> pom)

    pom
  }
}

/**
  * Parse POM XML file into constraint objects
  *
  * Standard Meaning of Version Specification
  *
  *    (,1.0]          x <= 1.0
  *    1.0             "Soft" requirement on 1.0 (>=1.0)
  *    [1.0]           Hard requirement on 1.0
  *    [1.2,1.3]       1.2 <= x <= 1.3
  *    [1.0,2.0)       1.0 <= x < 2.0
  *    [1.5,)          x >= 1.5
  *    (,1.0], [1.2,)  x <= 1.0 or x >= 1.2. Multiple sets are comma-separated
  *    (,1.1),(1.1,)   This excludes 1.1 if it is known not to work in combination with this library
  *
  * TODO
  *
  *  - exclusions
  *  - dependency in parent projects
  *  - version specified in parent dependencyManagement section
  *  - properties
  *  - multi-module/agregating projects
  *
  * Reference: [1] http://maven.apache.org/pom.html
  *            [2] http://docs.codehaus.org/display/MAVEN/Dependency+Mediation+and+Conflict+Resolution
  *
  */

class PomFile(currentPackage: MavenPackage, node: Node) {
  var parent: PomFile  = null
  var modules: Seq[PomFile] = null

  def groupId = currentPackage.groupId
  def artifactId = currentPackage.artifactId
  def version = currentPackage.version

  /**
    *  parse the pom file and return the dependency constraints as a set of package sets
    */
  def parse(scope: Scope = COMPILE): Set[Set[MavenPackage]] = {
    if (hasParent) parent = getParent
    if (hasModules) modules = getModules

    var constraints = (for {
      dep <- node \ "dependencies" \ "dependency"
      scopeP = (dep \ "scope").text
      if  (scopeP.isEmpty && scope == COMPILE) || scope == scopeP // compile is the default
      depSet <- parseDependency(dep)
    } yield depSet).toSet

    if (parent != null) constraints = parent.parse(scope) ++ constraints
    if (modules != null) constraints = (modules :\ constraints) { (m, acc) => m.parse(scope) ++ acc }

    constraints
  }

  /**
    * resolve version which can be a property, a range, or defined in parent file
    */
  private def resolveVersion(groupId: String, artifactId: String, ver: String) = ver match {
    case Property(prop) => VersionRange(Property.resolve(node)(prop))
    case VersionRange(range) => range
    case "" if parent != null => parent.managedVersionFor(groupId, artifactId)  // version specified in parent POM file
    case ver => throw new InvalidVersionFormat("Unknown version format: " + ver + " when parsing POM file of " + currentPackage)
  }

  /**
    * resolve artifact version specified in dependencyManagement section
    */
  def managedVersionFor(groupId: String, artifactId: String): VersionRange = {
    object SameArtifact {
      def unapply(dep: Node): Option[VersionRange] = {
        val gid = (dep \ "groupId").text
        val aid = (dep \ "artifactId").text
        val ver = resolveVersion(groupId, artifactId, (dep \ "version").text)

        if (groupId == gid && artifactId == aid)
          Some(ver)
        else None
      }
    }

    (node \ "dependencyManagement" \ "dependencies" \ "dependency") collectFirst {
      case SameArtifact(ver) => ver
    } match {
      case Some(v) => v
      case None =>
        println("Error: can't find version specification in parent for " + groupId + ":" + artifactId + " in " + currentPackage)
        defaultVersion(groupId, artifactId)
    }
  }

  /**
    * whether current POM file has a <parent> section
    */
  private def hasParent = (node \ "parent").length > 0

  /**
    * parse the parent section, download POM for parent and create a new PomFile instance
    */
  private def getParent = {
    val groupId = (node \ "parent" \ "groupId").text
    val artifactId = (node \ "parent" \ "artifactId").text
    val version = (node \ "parent" \ "version").text

    PomFile(MavenPackage(groupId, artifactId, version))
  }

  /**
    * whether current POM file has a <modules> section
    */
  private def hasModules = (node \ "modules").length > 0

  /**
    * parse the parent section, download POM for each module and create a new PomFile instance
    */
  private def getModules = {
    (node \ "modules" \ "module") map { module =>
      val artifactId = module.text
      // groupId and version are the same as the aggregating project
      PomFile(MavenPackage(groupId, artifactId, version))
    }
  }

  /**
    * if there's no default version for an artifact, use the default version
    */
  def defaultVersion(groupId: String, artifactId: String): VersionRange = {
    // SimpleRange(Version(0, 0, 0, "", 0))
    throw new InvalidVersionFormat("version unspecified for " + groupId + ":" + artifactId)
  }

  /**
    * parse a single dependency node in /project/dependencies into a set of packages
    * as disjunctive constraints
    */
  private def parseDependency(dep: Node) = {
    val groupId = (dep \ "groupId").text
    val artifactId = (dep \ "artifactId").text

    // version range specification can be a property
    val range = resolveVersion(groupId, artifactId, (dep \ "version").text)

    getAllVersions(groupId, artifactId) map { allVersions =>
      // filter old, illegal version numbers
      // print a warning
      val legalVersions = allVersions.filter { ver =>
        if (Version.unapply(ver).nonEmpty) true else {
          println("Error: unknown version format " + ver + " in meta data XML of " +
            groupId + ":" +artifactId)
          false
        }
      }

      val compatibleVersions = getCompatibleVersions(range, legalVersions)
      compatibleVersions.map(v => MavenPackage(groupId, artifactId, v)).toSet
    }
  }

  /**
    * TODO: use cache to avoid duplicate HTTP request and parsing
    */
  private def getAllVersions(groupId:String, artifactId:String) = {
    MavenFetcher.getMetaData(groupId, artifactId) map { metaData =>
      (XML.loadString(metaData) \ "versioning" \ "versions" \ "version") map (_.text)
    }
  }

  private def getCompatibleVersions(range: VersionRange, allVersions: Seq[String]) = {
    allVersions.filter(s => range.contains(Version(s)))
  }
}


object MavenPomParser {
  /**
    * Used to parse a given POM file
    */
  def apply(spec: String, scope: Scope) = {
    PomFile(spec).parse(scope).toSet
  }

  /**
    * Fetch POM from repository and parse
    */
  def apply(pkg: MavenPackage, scope: Scope = COMPILE) = {
    val pom = PomFile(pkg)
    if (pom != null) Some(pom.parse(scope).toSet) else None
  }
}
