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
 * Tests the {@link BundleInfo} class
 * @author Michel Kraemer
 */
@RunWith(classOf[JUnitRunner])
class BundleInfoSpec extends WordSpec with ShouldMatchers {
  "BundleInfo.Version" should {
    import BundleInfo.Version
    
    "be able to parse correctly" in {
      Version("1") should be (Version(1))
      Version("1.2") should be (Version(1, 2))
      Version("1.2.3") should be (Version(1, 2, 3))
      Version("1.2.3.something") should be (Version(1, 2, 3, "something"))
    }
    
    "complain about invalid version numbers" in {
      evaluating {Version("a")} should produce [InvalidBundleException]
      evaluating {Version("1.a")} should produce [InvalidBundleException]
      evaluating {Version("1.2.a")} should produce [InvalidBundleException]
      evaluating {Version("1.2.3a")} should produce [InvalidBundleException]
    }
  }
  
  "BundleInfo.VersionRange" should {
    import BundleInfo.{Version, VersionRange}
    
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
