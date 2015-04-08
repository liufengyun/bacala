package bacala.maven


import scala.xml.XML
import scala.xml.Node
import bacala.util.Cache
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

// create an POM file object
object PomFile {
  def apply(spec: String)(implicit fetcher: MavenPackage => Option[String]) = {
    val node = XML.loadString(spec)
    val artifactId = (node \ "artifactId").text.trim
    // version and groupId may be inherited from /project/parent
    val groupId = resolveGroupId(node)
    val version = resolveVersion(node)

    val pkg = MavenPackage(MavenArtifact(groupId, artifactId), version)
    new PomFile(pkg, node)(fetcher)
  }

  def resolveVersion(node: Node) = {
    val version = (node \ "version").text.trim

    if (!version.isEmpty) version else
      (node \ "parent" \ "version").text.trim
  }

  def resolveGroupId(node: Node) = {
    val groupId = (node \ "groupId").text.trim

    if (!groupId.isEmpty) groupId else
      (node \ "parent" \ "groupId").text.trim
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

class PomFile(val currentPackage: MavenPackage, val node: Node)(implicit fetcher: MavenPackage => Option[String]) {
  val parent: PomFile  = if (hasParent) loadParent else null
  var modules: Seq[PomFile] = null // only load modules when needed

  def groupId = currentPackage.groupId
  def artifactId = currentPackage.artifactId
  def version = currentPackage.version

  /** Parses the POM file and return the dependency constraints
    */
  def parse(aggregator: PomFile = null): MavenPomFile = {
    if (hasModules) modules = loadModules

    var constraints = doParse

    if (parent != null && (aggregator == null || parent.currentPackage != aggregator.currentPackage))
      constraints = constraints ++ parent.doParseParent

    if (modules != null)
      constraints = (modules :\ constraints) { (m, acc) => m.parse(this).deps ++: acc }

    val repos = (node \ "repositories" \ "repository") map (parseRepository)

    MavenPomFile(currentPackage, constraints, repos)
  }

  // get all parents in the chain
  def doParseParent: Iterable[MavenDependency] = {
    if (parent == null) doParse else doParse ++ parent.doParseParent
  }

  // only parse contents in current POM file
  def doParse = {
    (node \ "dependencies" \ "dependency") map (parseDependency)
  }

  /** Resolve version which can be a property, a range, or defined in parent file
    */
  private def resolveVersion(artifact: MavenArtifact, ver: String) = ver match {
    case Property(prop) =>
      Property.resolve(node, prop) match {
        case Some(v) => v
        case None =>
          println("Warning: can't resolve version specification $prop for $artifact in the parent POM of $currentPackage")
          defaultVersion(artifact)
      }
    case VersionRange(range) => ver
    case "" if parent != null =>
      parent.managedVersionFor(artifact) match {  // version specified in parent POM file
        case Some(ver) => ver
        case None =>
          println("Warning: can't find version specification in parent for " + artifact + " in the parent POM of " + currentPackage)
          defaultVersion(artifact)
      }
    case ver =>
      defaultVersion(artifact)
      // throw new InvalidVersionFormat("Unknown version format: " + ver + " when parsing POM file of " + currentPackage)
  }

  /** Resolves groupId which could be a property
    */
  private def resolveGroupId(gid: String) = gid match {
    case Property(prop) =>
      Property.resolve(node, prop) match {
        case Some(v) => v
        case None =>
          println(s"Error: failed to resolve groupId property $gid in $currentPackage - using $groupId")
          currentPackage.groupId // using groupId of current POM
      }
    case _ => gid
  }

  /** Resolves artifact version specified in dependencyManagement section
    */
  private def managedVersionFor(artifact: MavenArtifact): Option[String] = {
    val deps = (node \ "dependencyManagement" \ "dependencies" \ "dependency")
    deps.filter { dep =>
      artifact == MavenArtifact((dep \ "groupId").text.trim, (dep \ "artifactId").text.trim)
    }.map { dep =>
      resolveVersion(MavenArtifact(groupId, artifactId), (dep \ "version").text.trim)
    }.headOption
  }

  /** Whether current POM file has a <parent> section
    */
  private def hasParent = (node \ "parent").length > 0

  /** Parses the parent section, download POM for parent and create a new PomFile instance
    */
  private def loadParent = {
    val groupId = (node \ "parent" \ "groupId").text.trim
    val artifactId = (node \ "parent" \ "artifactId").text.trim
    val version = (node \ "parent" \ "version").text.trim

    fetcher(MavenPackage(MavenArtifact(groupId, artifactId), version)) map { spec=>
      PomFile(spec)(fetcher)
    } match {
      case Some(pom) => pom
      case None =>
        println("Error: failed to load parent POM for " + currentPackage)
        null
    }
  }

  /** Whether current POM file has a <modules> section
    */
  def hasModules = (node \ "modules" \ "module").length > 0

  /** Parses the parent section, download POM for each module and create a new PomFile instance
    */
  def loadModules = {
    (node \ "modules" \ "module").map({module =>
      val artifactId = module.text.trim
      // groupId and version are the same as the aggregating project
      fetcher(MavenPackage(MavenArtifact(groupId, artifactId), version)) map { spec=>
        PomFile(spec)(fetcher)
      } match {
        case Some(pom) => pom
        case None =>
          println("Error: failed to load module " + artifactId + " for " + currentPackage)
          null
      }
    }).filter(_ != null)
  }

  /** if there's no default version for an artifact, use the default version
    */
  def defaultVersion(artifact: MavenArtifact) = {
    val ver = if (artifact.groupId == groupId) version else "(0.0.0,)"
    println("Warning: version unspecified for " + artifact + " in " + currentPackage + " - using " + ver)
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
    val range = resolveVersion(MavenArtifact(groupId, artifactId), (dep \ "version").text.trim)

    // parse exclusions
    val exclusions = (dep \ "exclusions" \ "exclusion").map(parseExclusion _)

    MavenDependency(MavenArtifact(groupId, artifactId), range, exclusions, scope, optional)
  }

  /** Parses exclusion in dependency
    */
  private def parseExclusion(exclusion: Node) = {
    val groupId = (exclusion \ "groupId").text.trim
    val artifactId = (exclusion \ "artifactId").text.trim

    MavenArtifact(groupId, artifactId)
  }

  /**
    * Parse repositories in POM file
    */
  private def parseRepository(repo: Node) = {
    val id = (repo \ "id").text.trim
    val name = (repo \ "name").text.trim
    val url = (repo \ "url").text.trim

    MavenResolver(id, name, url)
  }
}

object MavenPomParser {
  /** Used to parse a given POM file
    *
    * Parser requires a fetcher to get POM files for parent and modules
    */
  def apply(spec: String, fetcher: MavenPackage => Option[String]): MavenPomFile = {
    PomFile(spec)(fetcher).parse()
  }
}
