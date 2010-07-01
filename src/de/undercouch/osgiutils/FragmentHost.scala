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
 * Information about a fragment's host
 * @author Michel Kraemer
 */
case class FragmentHost(@BeanProperty symbolicName: String,
  @BeanProperty version: VersionRange = VersionRange.Default,
  @BeanProperty extension: FragmentHost.Extension.Extension = FragmentHost.Extension.None) {
  override def toString(): String = symbolicName +
    (if (version != VersionRange.Default) ";version=\"" + version + "\"" else "") +
    (extension match {
      case FragmentHost.Extension.None => ""
      case FragmentHost.Extension.Framework => ";extension:=framework"
      case FragmentHost.Extension.BootClassPath => ";extension:=bootclasspath"
    })
}

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
