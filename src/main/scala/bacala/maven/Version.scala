package bacala.maven

import scala.util.matching.Regex

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
                if (this.qualifier == "" && that.qualifier != "") false else
                  if (this.qualifier != "" && that.qualifier == "") true else
                    if (this.qualifier > that.qualifier) false else
                      if (this.qualifier < that.qualifier) true else
                        if (this.build > that.build) false else
                          if (this.build < that.build) true else false
  }
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

/*
 * represents a range of versions
 *
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
 *    (,1.1), (1.1,)  This excludes 1.1 if it is known not to work in combination with this library
 *
 */
sealed abstract class VersionRange {
  def contains(version: Version): Boolean
}

object VersionRange  {
  import scala.util.matching.Regex

  def apply(s: String): VersionRange = s match {
    case SimpleRange(r) => r
    case OpenLeftRange(r) => r
    case OpenRightRange(r) => r
    case IntervalRange(r) => r
    case CompositeRange(r) => r
    case _ => throw new InvalidVersionFormat("Unknown verion range: " + s)
  }

  def unapply(s: String): Option[VersionRange] = try {
    Some(apply(s))
  } catch {
    case e: InvalidVersionFormat => None
  }
}

// e.g. 1.0
case class SimpleRange(version: Version) extends VersionRange {
  // NOTE: maven interprets 1.0 like >= 1.0
  override def contains(ver: Version) = this.version == ver || this.version < ver
}

object SimpleRange {
  def apply(ver: String): SimpleRange = SimpleRange(Version(ver))

  def unapply(s: String): Option[SimpleRange] = try {
    Some(apply(s))
  } catch {
    case e: InvalidVersionFormat => None
  }
}

// (,1.0] or (,1.1)
case class OpenLeftRange(max: Version, included: Boolean) extends VersionRange {
  // NOTE: maven interprets 1.0 like >= 1.0
  override def contains(ver: Version) = {
    (this.included && this.max == ver) || this.max > ver
  }
}

object OpenLeftRange {
  val openLeft1 = """^\s*\(\s*,\s*(\S+)\s*\)\s*$""".r
  val openLeft2 = """^\s*\(\s*,\s*(\S+)\s*\]\s*$""".r

  def apply(s: String): OpenLeftRange = s match {
    case openLeft1(ver) => OpenLeftRange(Version(ver), false)
    case openLeft2(ver) => OpenLeftRange(Version(ver), true)
    case _ => throw new InvalidVersionFormat("Unknown version range format: " + s)
  }

  def unapply(s: String): Option[OpenLeftRange] = try {
    Some(apply(s))
  } catch {
    case e: InvalidVersionFormat => None
  }
}

// [1.5,) or (1.1,)
case class OpenRightRange(min: Version, included: Boolean) extends VersionRange {
  override def contains(ver: Version) = {
    (this.included && this.min == ver) || this.min < ver
  }
}

object OpenRightRange {
  val openRight1 = """^\s*\(\s*(\S+)\s*,\s*\)\s*$""".r
  val openRight2 = """^\s*\[\s*(\S+)\s*,\s*\)\s*$""".r

  def apply(s: String): OpenRightRange = s match {
    case openRight1(ver) => OpenRightRange(Version(ver), false)
    case openRight2(ver) => OpenRightRange(Version(ver), true)
    case _ => throw new InvalidVersionFormat("Unknown version range format: " + s)
  }

  def unapply(s: String): Option[OpenRightRange] = try {
    Some(apply(s))
  } catch {
    case e: InvalidVersionFormat => None
  }
}


// [1.0,2.0)
case class IntervalRange(min: Version, max: Version, leftIncluded: Boolean, rightIncluded: Boolean) extends VersionRange {
  override def contains(ver: Version) = {
    (this.leftIncluded && this.min == ver) ||
    (this.rightIncluded && this.max == ver) ||
    (min < ver && ver < max)
  }
}

object IntervalRange {
  val interval1 = """^\s*\(\s*(\S+)\s*,\s*(\S+)\s*\)\s*$""".r
  val interval2 = """^\s*\[\s*(\S+)\s*,\s*(\S+)\s*\)\s*$""".r
  val interval3 = """^\s*\(\s*(\S+)\s*,\s*(\S+)\s*\]\s*$""".r
  val interval4 = """^\s*\[\s*(\S+)\s*,\s*(\S+)\s*\]\s*$""".r

  def apply(s: String): IntervalRange = s match {
    case interval1(min, max) => IntervalRange(Version(min), Version(max), false, false)
    case interval2(min, max) => IntervalRange(Version(min), Version(max), true, false)
    case interval3(min, max) => IntervalRange(Version(min), Version(max), false, true)
    case interval4(min, max) => IntervalRange(Version(min), Version(max), true, true)
    case _ => throw new InvalidVersionFormat("Unknown version range format: " + s)
  }

  def unapply(s: String): Option[IntervalRange] = try {
    Some(apply(s))
  } catch {
    case e: InvalidVersionFormat => None
  }
}

// (,1.0], [1.2,)
case class CompositeRange(intervals: List[VersionRange]) extends VersionRange {
  override def contains(ver: Version) = {
    this.intervals.exists(_.contains(ver))
  }
}

object CompositeRange {
  val rangePat = """[\(\[][^\]\)]{4,}[\)\]]""".r

  def apply(s: String) : CompositeRange = {
    CompositeRange((rangePat findAllIn s).toList.map{
      case OpenLeftRange(r) => r
      case OpenRightRange(r) => r
      case IntervalRange(r) => r
      case _ => throw new InvalidVersionFormat("Unknown version range format: " + s)
    })
  }

  def unapply(s: String): Option[CompositeRange] = try {
    val range = apply(s)
    if (range.intervals.length < 2) None
    else Some(range)
  } catch {
    case e: InvalidVersionFormat => None
  }
}
