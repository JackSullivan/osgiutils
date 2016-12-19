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

import org.scalatest.{WordSpec, Matchers}

/**
 * Tests the {@link FragmentHost} class
 * @author Michel Kraemer
 */
class FragmentHostSpec extends WordSpec with Matchers {
  "FragmentHostSpec" should {
    "have the right string representation" in {
      FragmentHost("A").toString should be ("A")
      FragmentHost("A", VersionRange(Version(2))).toString should
        be ("A;version=\"2\"")
      FragmentHost("A", VersionRange(Version(2)), FragmentHost.Extension.Framework).toString should
        be ("A;version=\"2\";extension:=framework")
      FragmentHost("A", VersionRange(Version(2)), FragmentHost.Extension.BootClassPath).toString should
        be ("A;version=\"2\";extension:=bootclasspath")
    }
  }
}
