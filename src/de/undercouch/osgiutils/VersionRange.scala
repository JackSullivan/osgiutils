// OSGi Utilities
// Copyright (c) 2010 Michel Kraemer
//
// This file is released under the terms of the MIT License.
// It is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the MIT License for more details.
//
// You should have received a copy of the MIT License along with
// this file; if not, goto http://www.michel-kraemer.de/en/mit-license

package de.undercouch.osgiutils

import scala.reflect.{BeanProperty, BooleanBeanProperty}
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input._

/**
 * A version range
 * @author Michel Kraemer
 */
case class VersionRange(@BeanProperty floor: Version, @BeanProperty ceiling: Version,
  @BooleanBeanProperty floorInclusive: Boolean,
  @BooleanBeanProperty ceilingInclusive: Boolean) {
  require(floor <= ceiling, "The higher version number must be greater or equal than the lower one")
  
  /**
   * Checks if this version range contains a given version
   * @param v the version
   * @return true if this version range contains v, false otherwise
   */
  def contains(v: Version): Boolean = {
    if (floorInclusive && ceilingInclusive)
      v >= floor && v <= ceiling
    else if (floorInclusive && !ceilingInclusive)
      v >= floor && v < ceiling
    else if (!floorInclusive && ceilingInclusive)
      v > floor && v <= ceiling
    else
      v > floor && v < ceiling
  }
}

/**
 * Defines methods to parse version ranges
 * @author Michel Kraemer
 */
object VersionRange {
  /**
   * A default version range from Version.Default (inclusively) to Version.Infinite
   */
  val Default = VersionRange(Version.Default)
  
  /**
   * Defines the new version range which that starts at the
   * given version number (inclusively) and goes to Version.Infinite
   * @param v the lower version number
   * @return the new version range
   */
  def apply(v: Version): VersionRange =
    VersionRange(v, Version.Infinite, true, false)
    
  /**
   * Parses a version range string
   * @param v the string to parse
   * @return the parsed version range
   * @throws InvalidBundleException if the version range string is invalid
   */
  def apply(v: String): VersionRange = {
    object RangeParser extends RegexParsers {
      lazy val range = interval | atleast
      lazy val interval = ( floorinc | floorexc ) ~ version ~ "," ~ version ~ ( ceilinc | ceilexc ) ^^ {
        case left ~ floor ~ "," ~ ceiling ~ right => VersionRange(floor, ceiling, left, right)
      }
      lazy val floorinc = "[" ^^^ true
      lazy val floorexc = "(" ^^^ false
      lazy val ceilinc = "]" ^^^ true
      lazy val ceilexc = ")" ^^^ false
      lazy val atleast = version ^^ { VersionRange(_, Version.Infinite, true, false) }
      lazy val version = regex("[0-9\\.a-zA-Z_-]+"r) ^^ Version.apply
    }
    
    RangeParser.range(new CharSequenceReader(v)) match {
      case RangeParser.Success(result, next) if next.atEnd => result
      case s => throw new InvalidBundleException("Invalid version range: " + v)
    }
  }
}
