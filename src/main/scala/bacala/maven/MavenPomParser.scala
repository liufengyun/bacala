package bacala.maven

/*
 * Parse POM XML file into constraint objects
 *
 * Reference: [1] http://maven.apache.org/pom.html
 *            [2] http://docs.codehaus.org/display/MAVEN/Dependency+Mediation+and+Conflict+Resolution
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
 *  - properties
 *  - parent/child projects
 *  - multi-module/agregating projects
 */

import scala.xml.XML
import scala.xml.Node

object MavenPomParser extends ((String, String) => Set[Set[MavenPackage]]) {
  override def apply(spec:String, scope:String="compile") = {
    val node = XML.loadString(spec)

    val constraints = for {
      dep <- node \ "dependencies" \ "dependency"
      scopeP = (dep \ "scope").text
      if  (scopeP == "" && scope == "compile") || scope == scopeP // compile is the default
    } yield parseDependency(dep)

    constraints.toSet
  }

  private def parseDependency(dep: Node) = {
    val groupId = (dep \ "groupId").text
    val artifactId = (dep \ "artifactId").text
    val version = (dep \ "version").text

    val allVersions = getAllVersions(groupId, artifactId)
    val compatibleVersions = getCompatibleVersions(VersionRange(version), allVersions)

    compatibleVersions.map(v => MavenPackage(groupId, artifactId, v)).toSet
  }

  private def getAllVersions(groupId:String, artifactId:String) = {
    val metaData = MavenFetcher.getMetaData(groupId, artifactId)
    val node = XML.loadString(metaData)

    (node \ "versioning" \ "versions" \ "version") map (_.text)
  }

  private def getCompatibleVersions(range: VersionRange, allVersions: Seq[String]) = {
    allVersions.filter(s => range.contains(Version(s)))
  }
}
