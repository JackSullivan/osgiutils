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
 * Tests the {@link RequiredBundle} class
 * @author Michel Kraemer
 */
class RequiredBundleSpec extends WordSpec with Matchers {
  "RequiredBundle" should {
    "have the right string representation" in {
      RequiredBundle("A").toString should be ("A")
      RequiredBundle("A", true).toString should
        be ("A;resolution:=optional")
      RequiredBundle("A", true, VersionRange(Version(2))).toString should
        be ("A;version=\"2\";resolution:=optional")
      RequiredBundle("A", true, VersionRange(Version(2)), true).toString should
        be ("A;version=\"2\";resolution:=optional;visibility:=reexport")
    }
  }
}
