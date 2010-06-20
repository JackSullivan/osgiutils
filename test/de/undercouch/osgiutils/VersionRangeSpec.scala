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
    
    "contain versions" in {
      var vr_def = VersionRange.Default
      vr_def contains Version.Default should be (true)
      vr_def contains Version(1) should be (true)
      
      val vr1 = VersionRange("[1.0, 2.0]")
      vr1 contains Version(1) should be (true)
      vr1 contains Version(1, 5) should be (true)
      vr1 contains Version(2) should be (true)
      
      val vr2 = VersionRange("(1.0, 2.0]")
      vr2 contains Version(1, 5) should be (true)
      vr2 contains Version(2) should be (true)
      
      val vr3 = VersionRange("[1.0, 2.0)")
      vr3 contains Version(1) should be (true)
      vr3 contains Version(1, 5) should be (true)
      
      val vr4 = VersionRange("(1.0, 2.0)")
      vr4 contains Version(1, 5) should be (true)
    }
    
    "not contain versions" in {
      val vr1 = VersionRange("[1.0, 2.0]")
      vr1 contains Version(0, 5) should be (false)
      vr1 contains Version(2, 5) should be (false)
      
      val vr2 = VersionRange("(1.0, 2.0]")
      vr2 contains Version(1) should be (false)
      vr2 contains Version(0, 5) should be (false)
      vr2 contains Version(2, 5) should be (false)
      
      val vr3 = VersionRange("[1.0, 2.0)")
      vr3 contains Version(0, 5) should be (false)
      vr3 contains Version(2, 5) should be (false)
      vr3 contains Version(2) should be (false)
      
      val vr4 = VersionRange("(1.0, 2.0)")
      vr4 contains Version(1) should be (false)
      vr4 contains Version(0, 5) should be (false)
      vr4 contains Version(2, 5) should be (false)
      vr4 contains Version(2) should be (false)
    }
  }
}
