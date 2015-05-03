package bacala.maven

import scala.xml.XML
import scala.xml.Node

import bacala.util.Cache
import bacala.util.ConsoleHelper.ColorText
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
  * Reference: http://maven.apache.org/pom.html#Properties
  */
object Property {
  val propertyPat = """\s*\$\{\s*([A-Za-z0-9.-]+)\s*\}\s*""".r

  def resolve(node: Node, property: String): Option[String] = {
    val res = doResolve(node, property)

    // property can reference another property
    res match {
      case Some(Property(prop)) => resolve(node, prop)
      case _ => res
    }
  }

  private def doResolve(node: Node, property: String) = {
    // first try variable property
    val res = resolveVariable(node, property) // variable property

    if (!res.isEmpty) Some(res) else {
      val parts = property.split('.')
      resolvePath(node, parts) // path property
    }
  }

  private def resolveVariable(node: Node, property: String) = {
    (node \ "properties" \ property ).text.trim
  }

  private def resolvePath(node: Node, path: Seq[String]): Option[String] = {
    val initial: Option[Node] = Some(node)
    // support old-style property like ${version}
    val parts = if (path(0) == "project") path.tail else path
    val result = (initial /: parts) { (opt, part) =>
      for {
        node <- opt
        seq = node \ part
        if seq.length > 0
      } yield seq(0)
    }

    result match {
      case Some(node) => Some(node.text.trim)
      case None =>
        if (parts(0) == "parent") None else
          resolvePath(node, "parent" +: parts)
    }
  }

  def unapply(s: String): Option[String] = s match {
    case propertyPat(prop) => Some(prop)
    case _ => None
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
  *
  * Reference: [1] http://maven.apache.org/pom.html
  *            [2] http://docs.codehaus.org/display/MAVEN/Dependency+Mediation+and+Conflict+Resolution
  *
  */

class PomFile(val node: Node) {
  type Fetcher = MPackage => Option[String]

  var parent: PomFile = null
  var modules: Seq[PomFile] = null

  // parse on initialization
  val repositories = (node \ "repositories" \ "repository") map (parseRepository)
  val currentPackage = parseBasicInfo(node)

  def groupId = currentPackage.groupId
  def artifactId = currentPackage.artifactId
  def version = currentPackage.version

  /** Parses the POM file and return the dependency constraints
    */
  def parse(aggregator: PomFile = null)(implicit fetcher: Fetcher): MDescriptor = {
    if (hasParent) parent = loadParent(fetcher)
    if (hasModules) modules = loadModules(fetcher)

    var constraints = doParse

    if (parent != null && (aggregator == null || parent.currentPackage != aggregator.currentPackage))
      constraints = constraints ++ parent.doParseParent

    if (modules != null)
      constraints = (modules :\ constraints) { (m, acc) => m.parse(this).deps ++: acc }

    MDescriptor(currentPackage, constraints, repositories)
  }

  // get all parents in the chain
  def doParseParent(implicit fetcher: Fetcher): Iterable[MDependency] = {
    // we need to load parent here because *parse* is not called for current instance
    if (parent == null && hasParent) parent = loadParent(fetcher)

    if (parent == null) doParse else doParse ++ parent.doParseParent
  }

  // only parse contents in current POM file
  def doParse = {
    (node \ "dependencies" \ "dependency") map (parseDependency)
  }

  /** Resolve version which can be a property, a range, or defined in parent file
    */
  private def resolveVersion(lib: MLib, ver: String) = ver match {
    case Property(prop) =>
      Property.resolve(node, prop) match {
        case Some(v) => v
        case None =>
          println("Warning: can't resolve version specification $prop for $lib in the parent POM of $currentPackage".yellow)
          defaultVersion(lib)
      }
    case VersionRange(range) => ver
    case "" if parent != null =>
      parent.managedVersionFor(lib) match {  // version specified in parent POM file
        case Some(ver) => ver
        case None =>
          println(("Warning: can't find version specification in parent for " + lib + " in the parent POM of " + currentPackage).yellow)
          defaultVersion(lib)
      }
    case ver =>
      defaultVersion(lib)
      // throw new InvalidVersionFormat("Unknown version format: " + ver + " when parsing POM file of " + currentPackage)
  }

  /** Resolves groupId which could be a property
    */
  private def resolveGroupId(gid: String) = gid match {
    case Property(prop) =>
      Property.resolve(node, prop) match {
        case Some(v) => v
        case None =>
          println(s"Error: failed to resolve groupId property $gid in $currentPackage - using $groupId".red)
          currentPackage.groupId // using groupId of current POM
      }
    case _ => gid
  }

  /** Resolves lib version specified in dependencyManagement section
    */
  private def managedVersionFor(lib: MLib): Option[String] = {
    val deps = (node \ "dependencyManagement" \ "dependencies" \ "dependency")
    deps.filter { dep =>
      lib == MLib((dep \ "groupId").text.trim, (dep \ "artifactId").text.trim)
    }.map { dep =>
      resolveVersion(MLib(groupId, artifactId), (dep \ "version").text.trim)
    }.headOption
  }

  /** Parses groupId, artifactId and version in POM file
    */
  def parseBasicInfo(node: Node) = {
    var version = (node \ "version").text.trim
    if (version.isEmpty)
      version = (node \ "parent" \ "version").text.trim

    var groupId = (node \ "groupId").text.trim
    if (groupId.isEmpty)
      groupId = (node \ "parent" \ "groupId").text.trim

    val artifactId = (node \ "artifactId").text.trim

    MPackage(MLib(groupId, artifactId), version)
  }

  /** Whether current POM file has a <parent> section
    */
  private def hasParent = (node \ "parent").length > 0

  /** Parses the parent section, download POM for parent and create a new PomFile instance
    */
  private def loadParent(fetcher: Fetcher) = {
    val groupId = (node \ "parent" \ "groupId").text.trim
    val artifactId = (node \ "parent" \ "artifactId").text.trim
    val version = (node \ "parent" \ "version").text.trim

    fetcher(MPackage(MLib(groupId, artifactId), version)) map { spec=>
      PomFile(spec)
    } match {
      case Some(pom) => pom
      case None =>
        println(("Error: failed to load parent POM for " + currentPackage).red)
        null
    }
  }

  /** Whether current POM file has a <modules> section
    */
  def hasModules = (node \ "modules" \ "module").length > 0

  /** Parses the parent section, download POM for each module and create a new PomFile instance
    */
  def loadModules(fetcher: Fetcher) = {
    (node \ "modules" \ "module").map({module =>
      val artifactId = module.text.trim
      // groupId and version are the same as the aggregating project
      fetcher(MPackage(MLib(groupId, artifactId), version)) map { spec=>
        PomFile(spec)
      } match {
        case Some(pom) => pom
        case None =>
          println(("Error: failed to load module " + artifactId + " for " + currentPackage).red)
          null
      }
    }).filter(_ != null)
  }

  /** if there's no default version for an lib, use the default version
    */
  def defaultVersion(lib: MLib) = {
    val ver = if (lib.groupId == groupId) version else "(0.0.0,)"
    println(("Warning: version unspecified for " + lib + " in " + currentPackage + " - using " + ver).yellow)
    ver
  }

  /** Parses a single dependency node in /project/dependencies
    */
  private def parseDependency(dep: Node) = {
    val groupId = resolveGroupId((dep \ "groupId").text.trim)
    val artifactId = (dep \ "artifactId").text.trim
    val scopeText = (dep \ "scope").text.trim
    val scope = if (scopeText.isEmpty) COMPILE else Scope(scopeText)
    val optional = (dep \ "optional").text.trim == "true"

    // version range specification can be a complex
    val range = resolveVersion(MLib(groupId, artifactId), (dep \ "version").text.trim)

    // parse exclusions
    val exclusions = (dep \ "exclusions" \ "exclusion").map(parseExclusion _)

    MDependency(MLib(groupId, artifactId), range, exclusions, scope, optional)
  }

  /** Parses exclusion in dependency
    */
  private def parseExclusion(exclusion: Node) = {
    val groupId = (exclusion \ "groupId").text.trim
    val artifactId = (exclusion \ "artifactId").text.trim

    MLib(groupId, artifactId)
  }

  /** Parses repositories in POM file
    */
  private def parseRepository(repo: Node) = {
    val id = (repo \ "id").text.trim
    val name = (repo \ "name").text.trim
    val url = (repo \ "url").text.trim

    MResolver(id, name, url)
  }
}

object PomFile {
  def apply(spec: String) = {
    val node = XML.loadString(spec)
    new PomFile(node)
  }
}

object MavenPomParser {
  type Fetcher = MPackage => Option[String]
  type FetcherMaker = Iterable[MResolver] => Fetcher

  /** Used to parse a given POM file
    *
    * Parser requires a fetcher to get POM files for parent and modules
    */
  def apply(spec: String, maker: FetcherMaker): MDescriptor = {
    val pom = PomFile(spec)

    implicit val fetcher = if (maker != null) maker(pom.repositories) else null
    pom.parse()
  }
}
