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

import org.scalatest._
import matchers.ShouldMatchers
import junit.JUnitRunner
import org.junit.runner.RunWith

/**
 * Tests the {@link VersionRange} class
 * @author Michel Kraemer
 */
@RunWith(classOf[JUnitRunner])
class VersionRangeSpec extends WordSpec with ShouldMatchers {
  "VersionRange" should {
    "be able to parse correctly" in {
      VersionRange("1.0") should be (VersionRange(Version(1), Version.Infinite, true, false))
      VersionRange("1.2.3") should be (VersionRange(Version(1, 2, 3), Version.Infinite, true, false))
      VersionRange("(1.0, 2.0)") should be (VersionRange(Version(1), Version(2), false, false))
      VersionRange("(1.0, 2.0]") should be (VersionRange(Version(1), Version(2), false, true))
      VersionRange("[1.0, 2.0)") should be (VersionRange(Version(1), Version(2), true, false))
      VersionRange("[1.0, 2.0]") should be (VersionRange(Version(1), Version(2), true, true))
      VersionRange("[1.0,2.0]") should be (VersionRange(Version(1), Version(2), true, true))
    }
    
    "complain about bad version range" in {
      evaluating {VersionRange("1a")} should produce [InvalidBundleException]
      evaluating {VersionRange("(1")} should produce [InvalidBundleException]
      evaluating {VersionRange("(1,")} should produce [InvalidBundleException]
      evaluating {VersionRange("1]")} should produce [InvalidBundleException]
    }
  }
}
