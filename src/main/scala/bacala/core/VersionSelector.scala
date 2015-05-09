package bacala.core

import scala.util.matching.Regex

abstract class VersionSelector {
  def choose(versions: Iterable[String]): Iterable[String]
}

case class RangeSelector(range: VersionRange) extends VersionSelector {
  def choose(versions: Iterable[String]) = {
    versions.filter(v => range.contains(Version(v)))
  }
}

/** Selects the latest version available
  */
object LatestSelector extends VersionSelector {
  def choose(versions: Iterable[String]): Iterable[String] = {
    Seq(versions.maxBy(Version(_)))
  }
}

/** Selects the latest version matching the prefix
  */
case class RangeLatestSelector(prefix: String) extends VersionSelector {
  def choose(versions: Iterable[String]): Iterable[String] = {
    Seq(versions.filter(_.startsWith(prefix)).maxBy(Version(_)))
  }
}

object VersionSelector  {
  val prefixLatest = """(\d(?:\.\d)?)\.\+""".r

  def apply(s: String): VersionSelector = s match {
    case prefixLatest(prefix) => RangeLatestSelector(prefix)
    case VersionRange(r) => RangeSelector(r)
    case "latest.integration" => LatestSelector
    case _ => throw new InvalidVersionFormat("Unknown verion constraint: " + s)
  }

  def unapply(s: String): Option[VersionSelector] = try {
    Some(apply(s))
  } catch {
    case e: InvalidVersionFormat => None
  }
}
