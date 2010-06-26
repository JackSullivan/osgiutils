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
  private def makeBundle(symbolicName: String, version: Version = Version.Default,
    fragmentHost: Option[FragmentHost] = None) =
    BundleInfo(null, 2, symbolicName, None, None, version, fragmentHost,
      Array.empty, Array.empty, Array.empty)
  
  "BundleRegistry" should {
    "not accept a bundle twice" in {
      val b1 = makeBundle("A");
      
      val reg = new BundleRegistry()
      reg.add(b1)
      evaluating { reg.add(b1) } should produce [IllegalStateException]
    }
    
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
    
    "find no fragments" in {
      val b2 = makeBundle("B", Version(1, 2, 3))
      val f1 = makeBundle("F1", fragmentHost = Some(FragmentHost("A")))
      val f2 = makeBundle("F2", fragmentHost = Some(FragmentHost("B", VersionRange(Version(2)))))
      
      val reg = new BundleRegistry()
      reg.add(b2)
      reg.add(f1)
      reg.add(f2)
      
      reg.findFragments(b2) should have size(0)
    }
    
    "find fragments" in {
      val b1 = makeBundle("A");
      val b2 = makeBundle("B", Version(1, 2, 3))
      val f1 = makeBundle("F1", fragmentHost = Some(FragmentHost("A")))
      val f2 = makeBundle("F2", fragmentHost = Some(FragmentHost("B", VersionRange(Version(1)))))
      
      val reg = new BundleRegistry()
      reg.add(f1)
      reg.add(f2)
      reg.add(b1)
      reg.add(b2)
      
      val bf1 = reg.findFragments(b1)
      bf1 should have size(1)
      bf1 should contain (f1)
      val bf2 = reg.findFragments(b2)
      bf2 should have size(1)
      bf2 should contain (f2)
    }
  }
}
