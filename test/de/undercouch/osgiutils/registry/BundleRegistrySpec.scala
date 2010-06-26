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
    fragmentHost: Option[FragmentHost] = None,
    exportedPackages: Array[ExportedPackage] = Array.empty,
    importedPackages: Array[ImportedPackage] = Array.empty,
    requiredBundles: Array[RequiredBundle] = Array.empty) =
    BundleInfo(null, 2, symbolicName, None, None, version, fragmentHost,
      exportedPackages, importedPackages, requiredBundles)
  
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
    
    "not find bundles by exported package" in {
      val b3 = makeBundle("C", version = Version(1), exportedPackages = Array(
          ExportedPackage("p"), ExportedPackage("q", Version(1)),
          ExportedPackage("r", mandatoryAttributes = Set("attr1"), matchingAttributes = Map("attr1" -> "value1")),
          ExportedPackage("s", matchingAttributes = Map("attr1" -> "value1", "attr2" -> "value2"))))
      
      val reg = new BundleRegistry()
      reg.add(b3)
      
      reg.findBundle(ImportedPackage("t")) should be (None)
      reg.findBundle(ImportedPackage("q", version = VersionRange(Version(2)))) should be (None)
      reg.findBundle(ImportedPackage("q", bundleSymbolicName = Some("A"))) should be (None)
      reg.findBundle(ImportedPackage("q", bundleSymbolicName = Some("C"),
          bundleVersion = VersionRange(Version(2)))) should be (None)
      reg.findBundle(ImportedPackage("r")) should be (None)
      reg.findBundle(ImportedPackage("s", matchingAttributes = Map("attr1" -> "value3"))) should be (None)
      reg.findBundle(ImportedPackage("s", matchingAttributes = Map("attr2" -> "value3"))) should be (None)
      reg.findBundle(ImportedPackage("s", matchingAttributes = Map("attr3" -> "value3"))) should be (None)
      reg.findBundle(ImportedPackage("s", matchingAttributes = Map("attr1" -> "value1", "attr2" -> "value3"))) should be (None)
      reg.findBundle(ImportedPackage("s", matchingAttributes = Map("attr1" -> "value2", "attr2" -> "value2"))) should be (None)
    }
    
    "find bundles by exported package" in {
      val b3 = makeBundle("C", version = Version(3), exportedPackages = Array(
          ExportedPackage("p"), ExportedPackage("q", Version(1)), ExportedPackage("r"),
          ExportedPackage("s", mandatoryAttributes = Set("attr1"), matchingAttributes = Map("attr1" -> "value1")),
          ExportedPackage("t", matchingAttributes = Map("attr1" -> "value1", "attr2" -> "value2"))))
      val b4 = makeBundle("D", version = Version(4), exportedPackages = Array(
          ExportedPackage("q", Version(2)), ExportedPackage("r"),
          ExportedPackage("s"),
          ExportedPackage("t", matchingAttributes = Map("attr3" -> "value3", "attr4" -> "value4"))))
      
      val reg = new BundleRegistry()
      reg.add(b3)
      reg.add(b4)
      
      reg.findBundle(ImportedPackage("p")) should be (Some(b3))
      reg.findBundle(ImportedPackage("q")) should be (Some(b3))
      reg.findBundle(ImportedPackage("q", version = VersionRange(Version(1)))) should be (Some(b3))
      reg.findBundle(ImportedPackage("q", version = VersionRange(Version(2)))) should be (Some(b4))
      reg.findBundle(ImportedPackage("q", bundleSymbolicName = Some("C"))) should be (Some(b3))
      reg.findBundle(ImportedPackage("q", bundleSymbolicName = Some("D"))) should be (Some(b4))
      reg.findBundle(ImportedPackage("q", bundleVersion = VersionRange(Version(3)))) should be (Some(b3))
      reg.findBundle(ImportedPackage("q", bundleVersion = VersionRange(Version(4)))) should be (Some(b4))
      reg.findBundle(ImportedPackage("s")) should be (Some(b4))
      reg.findBundle(ImportedPackage("s", matchingAttributes = Map("attr1" -> "value1"))) should be (Some(b3))
      reg.findBundle(ImportedPackage("t", matchingAttributes = Map("attr1" -> "value1"))) should be (Some(b3))
      reg.findBundle(ImportedPackage("t", matchingAttributes = Map("attr2" -> "value2"))) should be (Some(b3))
      reg.findBundle(ImportedPackage("t", matchingAttributes = Map("attr1" -> "value1", "attr2" -> "value2"))) should be (Some(b3))
      reg.findBundle(ImportedPackage("t", matchingAttributes = Map("attr3" -> "value3"))) should be (Some(b4))
      reg.findBundle(ImportedPackage("t", matchingAttributes = Map("attr4" -> "value4"))) should be (Some(b4))
      reg.findBundle(ImportedPackage("t", matchingAttributes = Map("attr3" -> "value3", "attr4" -> "value4"))) should be (Some(b4))
    }
  }
}
