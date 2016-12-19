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

import org.scalatest.{WordSpec, Matchers}
import de.undercouch.osgiutils._

/**
 * Tests the BundleRegistry
 * @author Michel Kraemer
 */
class BundleRegistrySpec extends WordSpec with Matchers {
  import BundleRegistry._
  import FrameworkConstants._
  
  private def makeBundle(symbolicName: String, version: Version = Version.Default,
    fragmentHost: Option[FragmentHost] = None,
    exportedPackages: List[ExportedPackage] = List.empty,
    importedPackages: List[ImportedPackage] = List.empty,
    requiredBundles: List[RequiredBundle] = List.empty) =
    BundleInfo(null, 2, symbolicName, None, None, version, fragmentHost,
      exportedPackages, importedPackages, requiredBundles)
  
  "BundleRegistry" should {
    "have a system bundle" in {
      val b1 = makeBundle("A", requiredBundles = List(RequiredBundle(SystemBundleSymbolicName)))
      
      val reg = new BundleRegistry()
      reg.add(b1)
      
      val r = reg.calculateRequiredBundles(b1)
      r should have size 1
      r.iterator.next.bundle.symbolicName should be (SystemBundleSymbolicName)
    }
    
    "have a system bundle that exports system packages" in {
      val oldpackages = System.getProperty(FrameworkSystemPackages)
      try {
        System.setProperty(FrameworkSystemPackages, "javax.mail,javax.ssl")
        
        val b1 = makeBundle("A", importedPackages = List(ImportedPackage("javax.ssl")))
      
        val reg = new BundleRegistry()
        reg.add(b1)
        
        val r = reg.calculateRequiredBundles(b1)
        r should have size 1
        r.iterator.next.bundle.symbolicName should be (SystemBundleSymbolicName)
      } finally {
        if (oldpackages != null)
          System.setProperty(FrameworkSystemPackages, oldpackages)
      }
    }
    
    "not accept a bundle twice" in {
      val b1 = makeBundle("A")
      
      val reg = new BundleRegistry()
      reg.add(b1)
      an [IllegalStateException] should be thrownBy { reg add b1 }
    }
    
    "not find a bundle" in {
      val b2 = makeBundle("B", Version(1, 2, 3))
      
      val reg = new BundleRegistry()
      reg.add(b2)
      
      reg.findBundle("A", VersionRange.Default) should be (None)
      reg.findBundle("B", VersionRange(Version(2))) should be (None)
    }
    
    "find a bundle" in {
      val b1 = makeBundle("A")
      val b2 = makeBundle("B", Version(1, 2, 3))
      
      val reg = new BundleRegistry()
      reg.add(b1)
      reg.add(b2)
      
      reg.findBundle("A", VersionRange.Default) should be (Some(b1))
      reg.findBundle("B", VersionRange(Version(1))) should be (Some(b2))
    }
    
    "find multiple bundles" in {
      val b1 = makeBundle("A", Version(1))
      val b2 = makeBundle("A", Version(2))
      
      val reg = new BundleRegistry()
      reg.add(b1)
      reg.add(b2)
      
      reg.findBundles("A", VersionRange.Default) should be (List(b2, b1))
    }
    
    "not find a required bundle" in {
      val b2 = makeBundle("B", Version(1, 2, 3))
      
      val reg = new BundleRegistry()
      reg.add(b2)
      
      reg.findBundle(RequiredBundle("A")) should be (None)
      reg.findBundle(RequiredBundle("B", version = VersionRange(Version(2)))) should be (None)
    }
    
    "find a required bundle" in {
      val b1 = makeBundle("A")
      val b2 = makeBundle("B", Version(1, 2, 3))
      
      val reg = new BundleRegistry()
      reg.add(b1)
      reg.add(b2)
      
      reg.findBundle(RequiredBundle("A")) should be (Some(b1))
      reg.findBundle(RequiredBundle("B", version = VersionRange(Version(1)))) should be (Some(b2))
    }
    
    "find multiple required bundles" in {
      val b1 = makeBundle("A", Version(1))
      val b2 = makeBundle("A", Version(2))
      
      val reg = new BundleRegistry()
      reg.add(b1)
      reg.add(b2)
      
      reg.findBundles(RequiredBundle("A")) should be (List(b2, b1))
    }
    
    "find no fragments" in {
      val b2 = makeBundle("B", Version(1, 2, 3))
      val f1 = makeBundle("F1", fragmentHost = Some(FragmentHost("A")))
      val f2 = makeBundle("F2", fragmentHost = Some(FragmentHost("B", VersionRange(Version(2)))))
      
      val reg = new BundleRegistry()
      reg.add(b2)
      reg.add(f1)
      reg.add(f2)
      
      reg.findFragments(b2) should have size 0
    }
    
    "find fragments" in {
      val b1 = makeBundle("A")
      val b2 = makeBundle("B", Version(1, 2, 3))
      val f1 = makeBundle("F1", fragmentHost = Some(FragmentHost("A")))
      val f2 = makeBundle("F2", fragmentHost = Some(FragmentHost("B", VersionRange(Version(1)))))
      
      val reg = new BundleRegistry()
      reg.add(f1)
      reg.add(f2)
      reg.add(b1)
      reg.add(b2)
      
      val bf1 = reg.findFragments(b1)
      bf1 should have size 1
      bf1 should contain (f1)
      val bf2 = reg.findFragments(b2)
      bf2 should have size 1
      bf2 should contain (f2)
    }
    
    "not find fragment hosts" in {
      val b2 = makeBundle("B", Version(1, 2, 3))
      
      val reg = new BundleRegistry()
      reg.add(b2)
      
      reg.findBundle(FragmentHost("A")) should be (None)
      reg.findBundle(FragmentHost("B", version = VersionRange(Version(2)))) should be (None)
    }
    
    "find fragment hosts" in {
      val b1 = makeBundle("A")
      val b2 = makeBundle("B", Version(1, 2, 3))
      
      val reg = new BundleRegistry()
      reg.add(b1)
      reg.add(b2)
      
      reg.findBundle(FragmentHost("A")) should be (Some(b1))
      reg.findBundle(FragmentHost("B", version = VersionRange(Version(1)))) should be (Some(b2))
    }
    
    "find multiple fragment hosts" in {
      val b1 = makeBundle("A", Version(1))
      val b2 = makeBundle("A", Version(2))
      
      val reg = new BundleRegistry()
      reg.add(b1)
      reg.add(b2)
      
      reg.findBundles(FragmentHost("A")) should be (List(b2, b1))
    }
    
    "not find bundles by exported package" in {
      val b3 = makeBundle("C", version = Version(1), exportedPackages = List(
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
      val b3 = makeBundle("C", version = Version(3), exportedPackages = List(
          ExportedPackage("p"), ExportedPackage("q", Version(1)), ExportedPackage("r"),
          ExportedPackage("s", mandatoryAttributes = Set("attr1"), matchingAttributes = Map("attr1" -> "value1")),
          ExportedPackage("t", matchingAttributes = Map("attr1" -> "value1", "attr2" -> "value2"))))
      val b4 = makeBundle("D", version = Version(4), exportedPackages = List(
          ExportedPackage("q", Version(2)), ExportedPackage("r"),
          ExportedPackage("s"),
          ExportedPackage("t", matchingAttributes = Map("attr3" -> "value3", "attr4" -> "value4"))))
      
      val reg = new BundleRegistry()
      reg.add(b3)
      reg.add(b4)
      
      reg.findBundle(ImportedPackage("p")) should be (Some(b3))
      reg.findBundle(ImportedPackage("q")) should be (Some(b4))
      reg.findBundle(ImportedPackage("q", version = VersionRange(Version(1)))) should be (Some(b4))
      reg.findBundle(ImportedPackage("q", version = VersionRange(Version(2)))) should be (Some(b4))
      reg.findBundle(ImportedPackage("q", bundleSymbolicName = Some("C"))) should be (Some(b3))
      reg.findBundle(ImportedPackage("q", bundleSymbolicName = Some("D"))) should be (Some(b4))
      reg.findBundle(ImportedPackage("q", bundleVersion = VersionRange(Version(3)))) should be (Some(b4))
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
    
    "find multiple bundles by exported package" in {
      val b1 = makeBundle("A", version = Version(1), exportedPackages = List(ExportedPackage("p")))
      val b2 = makeBundle("A", version = Version(2), exportedPackages = List(ExportedPackage("p")))
      
      val reg = new BundleRegistry()
      reg.add(b1)
      reg.add(b2)
      
      reg.findBundles(ImportedPackage("p")) should be (List(b2, b1))
    }
    
    "not calculate required bundles" in {
      val br2 = RequiredBundle("A")
      val b2 = makeBundle("B", requiredBundles = List(br2))
      val br3 = ImportedPackage("p")
      val b3 = makeBundle("C", importedPackages = List(br3))
      val br6 = FragmentHost("A")
      val b6 = makeBundle("F", fragmentHost = Some(br6))
      
      val reg = new BundleRegistry()
      
      reg.calculateRequiredBundles(b2) should be (Set(MissingRequiredBundle(b2, br2)))
      reg.calculateRequiredBundles(b3) should be (Set(MissingImportedPackage(b3, br3)))
      reg.calculateRequiredBundles(b6) should be (Set(MissingFragmentHost(b6, br6)))
    }
    
    "calculate required bundles" in {
      val b1 = makeBundle("A", exportedPackages = List(ExportedPackage("p")))
      val b2 = makeBundle("B", requiredBundles = List(RequiredBundle("A")))
      val b3 = makeBundle("C", importedPackages = List(ImportedPackage("p")))
      val b4 = makeBundle("D", requiredBundles = List(RequiredBundle("A", true)))
      val b5 = makeBundle("E", importedPackages = List(ImportedPackage("p", true)))
      val b6 = makeBundle("F", fragmentHost = Some(FragmentHost("A")))
      
      val ub1: ResolverResult = Unresolved(b1)
      
      val reg = new BundleRegistry()
      reg.add(b1)
      
      reg.calculateRequiredBundles(b2) should be (Set(ub1))
      reg.calculateRequiredBundles(b3) should be (Set(ub1))
      reg.calculateRequiredBundles(b4) should be (Set.empty)
      reg.calculateRequiredBundles(b4, true) should be (Set(ub1))
      reg.calculateRequiredBundles(b5) should be (Set.empty)
      reg.calculateRequiredBundles(b5, true) should be (Set(ub1))
      reg.calculateRequiredBundles(b6) should be (Set(ub1))
    }
    
    "not produce error on missing optional dependencies" in {
      val br2 = RequiredBundle("A", true)
      val b2 = makeBundle("B", requiredBundles = List(br2))
      val br3 = ImportedPackage("p", true)
      val b3 = makeBundle("C", importedPackages = List(br3))
      
      val reg = new BundleRegistry()
      
      reg.calculateRequiredBundles(b2) should be (Set.empty)
      reg.calculateRequiredBundles(b3) should be (Set.empty)
      reg.calculateRequiredBundles(b2, true) should be (Set.empty)
      reg.calculateRequiredBundles(b3, true) should be (Set.empty)
    }
    
    "calculate transitive dependencies" in {
      val b1 = makeBundle("A")
      val b2 = makeBundle("B", requiredBundles = List(RequiredBundle("A")))
      val b3 = makeBundle("C", requiredBundles = List(RequiredBundle("B")))
      val b4 = makeBundle("D", requiredBundles = List(RequiredBundle("B"), RequiredBundle("C")))
      
      val reg = new BundleRegistry()
      reg.add(b1)
      reg.add(b2)
      reg.add(b3)
      
      reg.calculateRequiredBundles(b3) should (
        have size (2) and
        contain (Unresolved(b1): ResolverResult) and
        contain (Unresolved(b2): ResolverResult)
      )
      
      reg.calculateRequiredBundles(b4) should (
        have size (3) and
        contain (Unresolved(b1): ResolverResult) and
        contain (Unresolved(b2): ResolverResult) and
        contain (Unresolved(b3): ResolverResult)
      )
    }
    
    "throw exception on dependency cycle" in {
      val b1 = makeBundle("A", requiredBundles = List(RequiredBundle("C")))
      val b2 = makeBundle("B", requiredBundles = List(RequiredBundle("A")))
      val b3 = makeBundle("C", requiredBundles = List(RequiredBundle("B")))
      
      val reg = new BundleRegistry()
      reg.add(b1)
      reg.add(b2)
      reg.add(b3)
      
      evaluating { reg.calculateRequiredBundles(b3) } should produce [DependencyCycleException]
      try {
        reg.calculateRequiredBundles(b3)
      } catch {
        case e: DependencyCycleException => e.path should be (Array(b3, b2, b1, b3))
      }
    }
    
    "resolve bundles" in {
      val b1 = makeBundle("A")
      val b2 = makeBundle("B", requiredBundles = List(RequiredBundle("A")))
      
      val reg = new BundleRegistry()
      
      reg.resolveBundle(b1) should be ('empty)
      
      reg.add(b1)
      reg.isResolved(b1) should be (false)
      reg.resolveBundles() should be ('empty)
      reg.isResolved(b1) should be (true)
      
      reg.resolveBundle(b2) should be ('empty)
      
      reg.add(b2)
      reg.isResolved(b1) should be (true)
      reg.isResolved(b2) should be (false)
      reg.resolveBundles() should be ('empty)
      reg.isResolved(b1) should be (true)
      reg.isResolved(b2) should be (true)
      
      val reg2 = new BundleRegistry()
      reg2.add(b1)
      reg2.add(b2)
      
      reg2.isResolved(b1) should be (false)
      reg2.isResolved(b2) should be (false)
      reg2.resolveBundles() should be ('empty)
      reg2.isResolved(b1) should be (true)
      reg2.isResolved(b2) should be (true)
    }
    
    "not resolve bundles" in {
      val b2 = makeBundle("B", requiredBundles = List(RequiredBundle("A")))
      
      val reg = new BundleRegistry()
      
      reg.resolveBundle(b2) should be (Set(MissingRequiredBundle(b2, RequiredBundle("A"))))
      
      reg.add(b2)
      reg.isResolved(b2) should be (false)
      reg.resolveBundles() should be (Set(MissingRequiredBundle(b2, RequiredBundle("A"))))
      reg.isResolved(b2) should be (false)
    }
    
    "be able to recover from resolver error" in {
      val b1 = makeBundle("A")
      val b2 = makeBundle("B", requiredBundles = List(RequiredBundle("A")))
      
      val reg = new BundleRegistry()
      reg.add(b2)
      
      reg.resolveBundles() should be (Set(MissingRequiredBundle(b2, RequiredBundle("A"))))
      
      reg.add(b1)
      
      reg.resolveBundles() should be ('empty)
    }
    
    "handle priorities correctly" in {
      val b1 = makeBundle("A", version = Version(1), exportedPackages = List(ExportedPackage("p")))
      val b2 = makeBundle("A", version = Version(2), exportedPackages = List(ExportedPackage("p")))
      val b3 = makeBundle("B", version = Version(2), exportedPackages = List(ExportedPackage("p")))
      
      val reg = new BundleRegistry()
      reg.add(b1)
      reg.add(b2)
      reg.add(b3)
      
      reg.resolveBundle(b2)
      reg.resolveBundle(b3)
      reg.isResolved(b1) should be (false)
      reg.isResolved(b2) should be (true)
      reg.isResolved(b3) should be (true)
      
      //resolved bundles should be preferred over unresolved ones
      //bundles added first (b2) should be preferred over last ones (b3)
      reg.findBundle(RequiredBundle("A")) should be (Some(b2))
      reg.findBundle(ImportedPackage("p")) should be (Some(b2))
      
      reg.resolveBundle(b1)
      reg.isResolved(b1) should be (true)
      reg.isResolved(b2) should be (true)
      reg.isResolved(b3) should be (true)
      
      //bundles with higher version should be preferred
      reg.findBundle(RequiredBundle("A")) should be (Some(b2))
      reg.findBundle(ImportedPackage("p")) should be (Some(b2))
    }
    
    "handle internal dependencies correctly" in {
      val b1 = makeBundle("A", version = Version(1), exportedPackages = List(ExportedPackage("p")),
        importedPackages = List(ImportedPackage("p")))
      
      val reg = new BundleRegistry()
      reg.add(b1)
      
      reg.resolveBundles()
    }

    /*
    "resolve uses conflict correctly" in {
      val C1 = makeBundle("C1", version = Version(1), exportedPackages = List(ExportedPackage("p")))
      val C2 = makeBundle("C2", version = Version(2), exportedPackages = List(ExportedPackage("p")))
      val B = makeBundle("B", importedPackages = List(ImportedPackage("p")),
          exportedPackages = List(ExportedPackage("q", uses = Set("p"))))
      val A = makeBundle("A", importedPackages = List(ImportedPackage("q"), ImportedPackage("p")))
      
      val reg = new BundleRegistry()
      reg.add(C1)
      reg.add(C2)
      reg.add(B)
      reg.add(A)
      
      reg.calculateRequiredBundles(A) should (
        have size 2 and
        contain (Unresolved(B): ResolverResult) and
        contain (Unresolved(C2): ResolverResult)
      )
      
      val B2 = makeBundle("B", importedPackages = List(ImportedPackage("p",
          bundleVersion = VersionRange(Version(1), Version(2), true, false))),
        exportedPackages = List(ExportedPackage("r", uses = Set("p"))))
      val A2 = makeBundle("A", importedPackages = List(ImportedPackage("r"), ImportedPackage("p")))
      
      reg.add(B2)
      reg.add(A2)
      
      reg.calculateRequiredBundles(A2) should (
        have size 2 and
        contain (Unresolved(B2): ResolverResult) and
        contain (Unresolved(C1): ResolverResult)
      )
    }
    */
  }
}
