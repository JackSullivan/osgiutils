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
 * Tests the {@link ImportedPackage} class
 * @author Michel Kraemer
 */
class ImportedPackageSpec extends WordSpec with Matchers {
  "ImportedPackage" should {
    "have the right string representation" in {
      ImportedPackage("p").toString should be ("p")
      ImportedPackage("p", true).toString should be ("p;resolution:=optional")
      ImportedPackage("p", true, VersionRange(Version(1))).toString should
        be ("p;resolution:=optional;version=\"1\"")
      ImportedPackage("p", bundleSymbolicName = Some("A")).toString should
        be ("p;bundle-symbolic-name=A")
      ImportedPackage("p", bundleVersion = VersionRange(Version(2))).toString should
        be ("p;bundle-version=\"2\"")
      ImportedPackage("p", matchingAttributes = Map("k1" -> "v1", "k2" -> "v2")).toString should
        be ("p;k1=\"v1\";k2=\"v2\"")
    }
  }
}
