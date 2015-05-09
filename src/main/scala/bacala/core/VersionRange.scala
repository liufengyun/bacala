package bacala.core

import scala.util.matching.Regex

/*
 * represents a range of versions
 *
 *
 * Standard Meaning of Version Specification in Maven
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
 * Standard Meaning of Version Specification in Ivy
 *
 *    [1.0,2.0]       1.0 <= x <= 2.0
 *    [1.0,2.0[       1.0 <= x < 2.0
 *    ]1.0,2.0]       1.0 < x <= 2.0
 *    ]1.0,2.0[       1.0 < x < 2.0
 *    [1.0,)          x >= 1.0
 *    ]1.0,)          x > 1.0
 *    (,2.0]          x <= 2.0
 *    (,2.0[          x < 2.0
 *    1.0.+           latest version in 1.0.*
 *    latest.integration  latest revision
 *
 * Reference: http://ant.apache.org/ivy/history/2.1.0/ivyfile/dependency.html
 */

abstract class VersionRange {
  def contains(version: Version): Boolean
}

object VersionRange  {
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

// (,1.0] or (,1.1) or (,2.0[
case class OpenLeftRange(max: Version, included: Boolean) extends VersionRange {
  // NOTE: maven interprets 1.0 like >= 1.0
  override def contains(ver: Version) = {
    (this.included && this.max == ver) || this.max > ver
  }
}

object OpenLeftRange {
  val openLeft1 = """\s*\(\s*,\s*(\S+)\s*\)\s*""".r
  val openLeft2 = """\s*\(\s*,\s*(\S+)\s*\]\s*""".r
  val openLeft3 = """\s*\(\s*,\s*(\S+)\s*\[\s*""".r

  def apply(s: String): OpenLeftRange = s match {
    case openLeft1(ver) => OpenLeftRange(Version(ver), false)
    case openLeft2(ver) => OpenLeftRange(Version(ver), true)
    case openLeft3(ver) => OpenLeftRange(Version(ver), false)
    case _ => throw new InvalidVersionFormat("Unknown version range format: " + s)
  }

  def unapply(s: String): Option[OpenLeftRange] = try {
    Some(apply(s))
  } catch {
    case e: InvalidVersionFormat => None
  }
}

// [1.5,) or (1.1,) or ]1.5,)
case class OpenRightRange(min: Version, included: Boolean) extends VersionRange {
  override def contains(ver: Version) = {
    (this.included && this.min == ver) || this.min < ver
  }
}

object OpenRightRange {
  val openRight1 = """^\s*\(\s*(\S+)\s*,\s*\)\s*$""".r
  val openRight2 = """^\s*\[\s*(\S+)\s*,\s*\)\s*$""".r
  val openRight3 = """^\s*\]\s*(\S+)\s*,\s*\)\s*$""".r

  def apply(s: String): OpenRightRange = s match {
    case openRight1(ver) => OpenRightRange(Version(ver), false)
    case openRight2(ver) => OpenRightRange(Version(ver), true)
    case openRight3(ver) => OpenRightRange(Version(ver), false)
    case _ => throw new InvalidVersionFormat("Unknown version range format: " + s)
  }

  def unapply(s: String): Option[OpenRightRange] = try {
    Some(apply(s))
  } catch {
    case e: InvalidVersionFormat => None
  }
}


// [1.0,2.0), [1.0, 2.0], (1.0, 2.0), (1.0, 2.0), [2.0]
// ]1.0, 2.0], [1.0, 2.0[, ]1.0, 2.0[
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
  val interval5 = """^\s*\[\s*(\S+)\s*]\s*$""".r
  val interval6 = """^\s*\]\s*(\S+)\s*,\s*(\S+)\s*\]\s*$""".r
  val interval7 = """^\s*\[\s*(\S+)\s*,\s*(\S+)\s*\[\s*$""".r
  val interval8 = """^\s*\]\s*(\S+)\s*,\s*(\S+)\s*\[\s*$""".r

  def apply(s: String): IntervalRange = s match {
    case interval1(min, max) =>
      IntervalRange(Version(min), Version(max), false, false)
    case interval2(min, max) =>
      IntervalRange(Version(min), Version(max), true, false)
    case interval3(min, max) =>
      IntervalRange(Version(min), Version(max), false, true)
    case interval4(min, max) =>
      IntervalRange(Version(min), Version(max), true, true)
    case interval5(v) =>
      IntervalRange(Version(v), Version(v), true, true)
    case interval6(min, max) =>
      IntervalRange(Version(min), Version(max), false, true)
    case interval7(min, max) =>
      IntervalRange(Version(min), Version(max), true, false)
    case interval8(min, max)=>
      IntervalRange(Version(min), Version(max), false, false)
    case _ =>
      throw new InvalidVersionFormat("Unknown version range format: " + s)
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
