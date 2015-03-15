package bacala.maven

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.concurrent.TrieMap

import bacala._

case class MavenPackage(val groupId:String, val artifactId:String, val version:String) extends Package {
  override def hashCode = {
    (List(groupId, artifactId, version) :\ 1) { (field, r) => r + 31*field.hashCode }
  }

  override def equals(other: Any) = other match {
    case MavenPackage(group, artifact, ver) =>
      this.groupId == group && this.artifactId == artifact && this.version == ver
    case _ => false
  }
}

/*
 * represents maven versioning scheme
 *
 * Reference:
 *   [1]http://docs.oracle.com/middleware/1212/core/MAVEN/maven_version.htm#MAVEN402
 *   [2] http://docs.codehaus.org/display/MAVEN/Dependency+Mediation+and+Conflict+Resolution
 *
 * TODO: SNAPSHOT versioning
 */
case class Version(major:Int, minor:Int, revision:Int, qualifier:String, build:Int) {
  def > (that: Version) = that < this

  def < (that: Version) = {
    if (this.major > that.major) false else
      if (this.major < that.major) true else
        if (this.minor > that.minor) false else
          if (this.minor < that.minor) true else
            if (this.revision > that.revision) false else
              if (this.revision < that.revision) true else
                if (this.qualifier > that.qualifier) false else
                  if (this.qualifier < that.qualifier) true else
                    if (this.build > that.build) false else
                      if (this.build < that.build) true else false
  }

  override def equals(other: Any) = other match {
    case v:Version =>
      !(this > v) && !(this < v)
    case _ => false
  }
}

object Version {
  import scala.util.matching.Regex

  // <major>.<minor>[.<revision>]([ -<qualififer> ] | [ -<build> ])
  val simple     =   """(\d+).(\d+)""".r
  val triple     =   """(\d+).(\d+).(\d+)""".r
  val build      =   """(\d+).(\d+).(\d+)-(\d+)""".r
  val qualifier  =   """(\d+).(\d+).(\d+)-(\w+)""".r
  val full       =   """(\d+).(\d+).(\d+)-(\w+)-(\d+)""".r

  def apply(ver: String): Version = ver match {
    case simple(major, minor) =>
      Version(major.toInt, minor.toInt, 0, "", 0)
    case triple(major, minor, revision) =>
      Version(major.toInt, minor.toInt, revision.toInt, "", 0)
    case build(major, minor, revision, build) =>
      Version(major.toInt, minor.toInt, revision.toInt, "", build.toInt)
    case qualifier(major, minor, revision, qualifier) =>
      Version(major.toInt, minor.toInt, revision.toInt, qualifier, 0)
    case full(major, minor, revision, qualifier, build) =>
      Version(major.toInt, minor.toInt, revision.toInt, qualifier, build.toInt)
    case _ => throw new Exception("Unknown versioning format: " + ver)
  }
}


class MavenRepository extends Repository {
  type PackageT = MavenPackage
  type PackagesT = Set[PackageT]
  type ConflictsT = Map[PackageT, PackageT]
  type ConstraintsT = Set[Set[PackageT]]
  type Fetcher = PackageT => ConstraintsT

  private val dependencies = new TrieMap[PackageT, ConstraintsT]

  // recursively fetch the dependency closure
  // TODO : use Future to fetch in asynchronous and in parallel
  def construct(initial: ConstraintsT, fetcher: Fetcher): Unit = {
    for {
      disjunctiveSet <- initial
      p <- disjunctiveSet
      if dependencies.putIfAbsent(p, Set()) == null // atomic
    }  {
      val constraints = fetcher(p)
      dependencies.put(p, constraints)
      construct(constraints, fetcher)
    }
  }

  override def apply(p: PackageT) = dependencies(p)

  override def packages = dependencies.keys.toSet

  override def conflicts = ???
}
