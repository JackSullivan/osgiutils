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

/**
 * Information about a fragment's host
 * @author Michel Kraemer
 */
case class FragmentHost(symbolicName: String, version: VersionRange = VersionRange.Default,
  extension: FragmentHost.Extension.Extension = FragmentHost.Extension.None)

/**
 * Static definitions for the information about fragment hosts
 * @author Michel Kraemer
 */
object FragmentHost {
  /**
   * Enumeration values for the extension mechanism
   * @author Michel Kraemer
   */
  object Extension extends Enumeration {
    type Extension = Value
    val None, Framework, BootClassPath = Value
  }
}
