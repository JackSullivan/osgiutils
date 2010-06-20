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

import scala.reflect.BeanProperty

/**
 * A version declaration (in the format major.minor.micro.qualifier)
 * @author Michel Kraemer
 */
case class Version(@BeanProperty major: Int, @BeanProperty minor: Int,
  @BeanProperty micro: Int, @BeanProperty qualifier: Option[String])

/**
 * Defines methods to parse version declarations
 * @author Michel Kraemer
 */
object Version {
  /**
   * The default version (0.0.0)
   */
  val Default = Version(0, 0, 0, None)
  
  /**
   * A version number that is infinitely large
   */
  val Infinite = new Version(0, 0, 0, None) {
    //TODO overwrite comparison operators
  }
  
  /**
   * Creates a new version number
   * @param major the major version number
   * @return the version number
   */
  def apply(major: Int): Version =
    Version(major, 0, 0, None)
  
  /**
   * Creates a new version number
   * @param major the major version number
   * @param minor the minor version number
   * @return the version number
   */
  def apply(major: Int, minor: Int): Version =
    Version(major, minor, 0, None)
    
  /**
   * Creates a new version number
   * @param major the major version number
   * @param minor the minor version number
   * @param micro the micro version number
   * @return the version number
   */
  def apply(major: Int, minor: Int, micro: Int): Version =
    Version(major, minor, micro, None)
  
  /**
   * Creates a new version number
   * @param major the major version number
   * @param minor the minor version number
   * @param micro the micro version number
   * @param qualifier the version qualifier
   * @return the version number
   */
  def apply(major: Int, minor: Int, micro: Int, qualifier: String): Version =
    Version(major, minor, micro, if (qualifier != null) Some(qualifier) else None)
    
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
          if (n.length > 3) Some(n(3)) else None
      )
    } catch {
      case _: NumberFormatException =>
        throw new InvalidBundleException("Invalid version number: " + v)
    }
  }
}
