package bacala.core

import scala.util.matching.Regex

/*
 * represents universal versioning scheme
 *
 * Reference:
 *   [1]http://docs.oracle.com/middleware/1212/core/MAVEN/maven_version.htm#MAVEN402
 *   [2] http://docs.codehaus.org/display/MAVEN/Dependency+Mediation+and+Conflict+Resolution
 *
 * TODO: SNAPSHOT versioning
 */
case class Version(major:Int, minor:Int, revision:Int, qualifier:String, build:Int) extends Ordered[Version] {
  def compare(that: Version) = {
    if (this.major > that.major) 1 else
      if (this.major < that.major) -1 else
        if (this.minor > that.minor) 1 else
          if (this.minor < that.minor) -1 else
            if (this.revision > that.revision) 1 else
              if (this.revision < that.revision) -1 else
                if (this.qualifier == "" && that.qualifier != "") 1 else
                  if (this.qualifier != "" && that.qualifier == "") -1 else
                    if (this.qualifier > that.qualifier) 1 else
                      if (this.qualifier < that.qualifier) -1 else
                        if (this.build > that.build) 1 else
                          if (this.build < that.build) -1 else 0
  }

  override def toString = s"$major.$minor.$revision-$qualifier-$build"
}

class InvalidVersionFormat(msg: String) extends Exception(msg)

object Version {

  // <major>.<minor>[.<revision>]([ -<qualififer> ] | [ -<build> ])
  val simple     =   """(\d+)\.(\d+)""".r
  val triple     =   """(\d+)\.(\d+)\.(\d+)""".r
  val build      =   """(\d+)\.(\d+)\.(\d+)-(\d+)""".r
  val qualifier  =   """(\d+)\.(\d+)\.(\d+)-(\w+)""".r
  val full       =   """(\d+)\.(\d+)\.(\d+)-(\w+)-(\d+)""".r

  // unstandard: 2.7.3.RC1
  val druple     =   """(\d+)\.(\d+)\.(\d+)\.(\w+)""".r

  // unstandard: 2.8.0.Beta1-RC1
  // 2.10.0-M1-virtualized.rdev-4217-2012-01-24-g9118644
  val wildcard   =   """(\d+)\.(\d+)\.(\d+)(?:\.|-)(.+)""".r

  // unstandard: 1.0-b1, 1.0-b1.1, 2.0b4
  val double    =   """(\d+)\.(\d+)[-_.]?(.+)""".r

  // unstandard: 1
  val number     =   """(\d+)""".r

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
    case druple(major, minor, revision, qualifier) =>
      Version(major.toInt, minor.toInt, revision.toInt, qualifier, 0)
    case wildcard(major, minor, revision, qualifier) =>
      Version(major.toInt, minor.toInt, revision.toInt, qualifier, 0)
    case double(major, minor, qualifier) =>
      Version(major.toInt, minor.toInt, 0, qualifier, 0)
    case number(major) =>
      Version(major.toInt, 0, 0, "", 0)
    case _ => throw new InvalidVersionFormat("Unknown version format: " + ver)
  }

  def unapply(s: String): Option[Version] = try {
    Some(apply(s))
  } catch {
    case e: Exception => None
  }
}
