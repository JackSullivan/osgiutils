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
 * Tests the {@link Version} class
 * @author Michel Kraemer
 */
@RunWith(classOf[JUnitRunner])
class VersionSpec extends WordSpec with ShouldMatchers {
  "Version" should {
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
    
    "be comparable" in {
      Version(2) > Version(1) should be (true)
      Version(1) < Version(2) should be (true)
      Version(1) <= Version(2) should be (true)
      Version(2) >= Version(1) should be (true)
      Version(2) <= Version(2) should be (true)
      Version(2) >= Version(2) should be (true)
      
      Version(2) < Version(1) should be (false)
      Version(1) > Version(2) should be (false)
      Version(1) >= Version(2) should be (false)
      Version(2) <= Version(1) should be (false)
      
      Version(1, 2) > Version(1, 1) should be (true)
      Version(1, 1) < Version(1, 2) should be (true)
      Version(1, 1) <= Version(1, 2) should be (true)
      Version(1, 2) >= Version(1, 1) should be (true)
      Version(1, 2) <= Version(1, 2) should be (true)
      Version(1, 2) >= Version(1, 2) should be (true)
      
      Version(1, 2) < Version(1, 1) should be (false)
      Version(1, 1) > Version(1, 2) should be (false)
      Version(1, 1) >= Version(1, 2) should be (false)
      Version(1, 2) <= Version(1, 1) should be (false)
      
      Version(1, 1, 2) > Version(1, 1, 1) should be (true)
      Version(1, 1, 1) < Version(1, 1, 2) should be (true)
      Version(1, 1, 1) <= Version(1, 1, 2) should be (true)
      Version(1, 1, 2) >= Version(1, 1, 1) should be (true)
      Version(1, 1, 2) <= Version(1, 1, 2) should be (true)
      Version(1, 1, 2) >= Version(1, 1, 2) should be (true)
      
      Version(1, 1, 2) < Version(1, 1, 1) should be (false)
      Version(1, 1, 1) > Version(1, 1, 2) should be (false)
      Version(1, 1, 1) >= Version(1, 1, 2) should be (false)
      Version(1, 1, 2) <= Version(1, 1, 1) should be (false)
      
      Version(1, 1, 1, "b") > Version(1, 1, 1, "a") should be (true)
      Version(1, 1, 1, "a") < Version(1, 1, 1, "b") should be (true)
      Version(1, 1, 1, "a") <= Version(1, 1, 1, "b") should be (true)
      Version(1, 1, 1, "b") >= Version(1, 1, 1, "a") should be (true)
      Version(1, 1, 1, "b") <= Version(1, 1, 1, "b") should be (true)
      Version(1, 1, 1, "b") >= Version(1, 1, 1, "b") should be (true)
      
      Version(1, 1, 1, "b") < Version(1, 1, 1, "a") should be (false)
      Version(1, 1, 1, "a") > Version(1, 1, 1, "b") should be (false)
      Version(1, 1, 1, "a") >= Version(1, 1, 1, "b") should be (false)
      Version(1, 1, 1, "b") <= Version(1, 1, 1, "a") should be (false)
      
      Version(2) > Version(1, 1, 1) should be (true)
      Version(1, 1, 2) < Version(2) should be (true)
      Version(1, 1, 2) <= Version(2) should be (true)
      Version(2) >= Version(1, 1, 2) should be (true)
      Version(1, 1, 2) <= Version(2) should be (true)
      Version(2) >= Version(1, 1, 2) should be (true)
      
      Version(1, 1, 1, "b") > Version(1, 1, 1) should be (true)
      Version(1, 1, 1) < Version(1, 1, 1, "b") should be (true)
      Version(1, 1, 1) <= Version(1, 1, 1, "b") should be (true)
      Version(1, 1, 1, "b") >= Version(1, 1, 1) should be (true)
      Version(1, 1, 1) <= Version(1, 1, 1, "b") should be (true)
      Version(1, 1, 1, "b") >= Version(1, 1, 1) should be (true)
      
      Version.Infinite > Version(1) should be (true)
      Version.Infinite > Version.Default should be (true)
      Version.Infinite > Version.Infinite should be (true)
      
      Version(1) < Version.Infinite should be (true)
    }
  }
}
