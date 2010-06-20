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

import scala.math.Ordered
import scala.reflect.BeanProperty

/**
 * A version declaration (in the format major.minor.micro.qualifier)
 * @author Michel Kraemer
 */
case class Version(@BeanProperty major: Int = 0, @BeanProperty minor: Int = 0,
  @BeanProperty micro: Int = 0, @BeanProperty qualifier: String = "") extends Ordered[Version] {
  require(qualifier != null, "Version qualifier must not be null")
  
  override def compare(that: Version): Int = {
    if (that eq Version.Infinite) {
      //every version number is lower than the infinite one
      -1
    } else {
      val mj = major - that.major
      if (mj != 0) {
        mj
      } else {
        val mi = minor - that.minor
        if (mi != 0) {
          mi
        } else {
          val mc = micro - that.micro
          if (mc != 0) {
            mc
          } else {
            qualifier.compareTo(that.qualifier)
          }
        }
      }
    }
  }
}

/**
 * Defines methods to parse version declarations
 * @author Michel Kraemer
 */
object Version {
  /**
   * The default version (0.0.0)
   */
  val Default = Version()
  
  /**
   * A version number that is infinitely large
   */
  val Infinite = new Version() {
    //the infinite version is always larger than anything else
    override def compare(that: Version): Int = 1
  }
    
  /**
   * Parses a version number string
   * @param v the string to parse
   * @return the version number
   * @throws InvalidBundleException if the version number string is invalid
   */
  def apply(v: String): Version = {
    val n = v.split("\\.")
    try {
      Version(
          if (n.length > 0) n(0).toInt else 0,
          if (n.length > 1) n(1).toInt else 0,
          if (n.length > 2) n(2).toInt else 0,
          if (n.length > 3) n(3) else ""
      )
    } catch {
      case _: NumberFormatException =>
        throw new InvalidBundleException("Invalid version number: " + v)
    }
  }
}
