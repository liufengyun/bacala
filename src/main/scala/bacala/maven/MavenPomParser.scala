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
    val parts = property.split('.')
    if (parts.length == 1) resolveVariable(node, parts(0)) // variable property
    else resolvePath(node, parts) // path property
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
  *  - properties
  *  - multi-module/agregating projects
  *
  * Reference: [1] http://maven.apache.org/pom.html
  *            [2] http://docs.codehaus.org/display/MAVEN/Dependency+Mediation+and+Conflict+Resolution
  *
  */
object MavenPomParser extends ((String, Scope) => Set[Set[MavenPackage]]) {

  override def apply(spec:String, scope:Scope=COMPILE) = {
    val node = XML.loadString(spec)

    val constraints = for {
      dep <- node \ "dependencies" \ "dependency"
      scopeP = (dep \ "scope").text
      if  (scopeP.isEmpty && scope == COMPILE) || scope == scopeP // compile is the default
      depSet <- parseDependency(dep, Property.resolve(node))
    } yield depSet

    constraints.toSet
  }


  private def parseDependency(dep: Node, propertyResolver: String => String) = {
    val groupId = (dep \ "groupId").text
    val artifactId = (dep \ "artifactId").text

    // version range specification can be a property
    val range = (dep \ "version").text match {
      case Property(prop) => VersionRange(propertyResolver(prop))
      case VersionRange(range) => range
      case "" => SimpleRange(Version(0, 0, 0, "", 0)) // version unspecified
      case ver => throw new InvalidVersionFormat("Unknown version format: " + ver)
    }

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

  private def getAllVersions(groupId:String, artifactId:String) = {
    MavenFetcher.getMetaData(groupId, artifactId) map { metaData =>
      (XML.loadString(metaData) \ "versioning" \ "versions" \ "version") map (_.text)
    }
  }

  private def getCompatibleVersions(range: VersionRange, allVersions: Seq[String]) = {
    allVersions.filter(s => range.contains(Version(s)))
  }
}
