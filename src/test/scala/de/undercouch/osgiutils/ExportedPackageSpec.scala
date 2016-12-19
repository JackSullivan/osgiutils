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
 * Tests the {@link ExportedPackage} class
 * @author Michel Kraemer
 */
class ExportedPackageSpec extends WordSpec with Matchers {
  "ExportedPackage" should {
    "have the right string representation" in {
      ExportedPackage("p").toString should be ("p")
      ExportedPackage("p", Version(1)).toString should
        be ("p;version=\"1\"")
      ExportedPackage("p", Version(1), uses = Set("p", "q")).toString should
        be ("p;version=\"1\";uses:=\"p,q\"")
      ExportedPackage("p", Version(1), mandatoryAttributes = Set("a1", "a2")).toString should
        be ("p;version=\"1\";mandatory:=a1,a2")
      ExportedPackage("p", Version(1), includedClasses = Set("a.A", "a.B")).toString should
        be ("p;version=\"1\";include:=a.A,a.B")
      ExportedPackage("p", Version(1), excludedClasses = Set("a.A", "a.B")).toString should
        be ("p;version=\"1\";exclude:=a.A,a.B")
      ExportedPackage("p", matchingAttributes = Map("k1" -> "v1", "k2" -> "v2")).toString should
        be ("p;k1=\"v1\";k2=\"v2\"")
    }
  }
}
