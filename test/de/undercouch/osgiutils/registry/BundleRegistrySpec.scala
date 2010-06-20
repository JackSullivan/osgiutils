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

package de.undercouch.osgiutils.registry

import org.scalatest._
import matchers.ShouldMatchers
import junit.JUnitRunner
import org.junit.runner.RunWith
import de.undercouch.osgiutils._

/**
 * Tests the BundleRegistry
 * @author Michel Kraemer
 */
@RunWith(classOf[JUnitRunner])
class BundleRegistrySpec extends WordSpec with ShouldMatchers {
  private def makeBundle(symbolicName: String, version: Version = Version.Default) =
    BundleInfo(null, 2, symbolicName, None, None, version, None,
      Array.empty, Array.empty, Array.empty)
  
  "BundleRegistry" should {
    "not find a required bundle" in {
      val b2 = makeBundle("B", Version(1, 2, 3))
      
      val reg = new BundleRegistry()
      reg.add(b2)
      
      reg.findBundle(RequiredBundle("A")) should be (None)
      reg.findBundle(RequiredBundle("B", version = VersionRange(Version(2)))) should be (None)
    }
    
    "find a required bundle" in {
      val b1 = makeBundle("A");
      val b2 = makeBundle("B", Version(1, 2, 3))
      
      val reg = new BundleRegistry()
      reg.add(b1)
      reg.add(b2)
      
      reg.findBundle(RequiredBundle("A")) should be (Some(b1))
      reg.findBundle(RequiredBundle("B", version = VersionRange(Version(1)))) should be (Some(b2))
    }
  }
}
