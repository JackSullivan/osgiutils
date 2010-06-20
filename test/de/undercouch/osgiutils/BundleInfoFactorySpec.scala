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
 * Tests the {@link BundleInfoFactory}
 * @author Michel Kraemer
 */
@RunWith(classOf[JUnitRunner])
class BundleInfoFactorySpec extends WordSpec with ShouldMatchers {
  "BundleInfoFactory" should {
    val bi = BundleInfoFactory.createBundleInfo(getClass.getResource("MANIFEST.MF"))
    
    "create a valid bundle info" in {
      bi.symbolicName should be ("de.undercouch.osgiutils.testbundle")
    }
    
    "parse the bundle's version number correctly" in {
      bi.version should be (Version(1, 2, 3, "something"))
    }
    
    "parse the bundle manifest version correctly" in {
      bi.manifestVersion should be (2)
    }
    
    "parse human readable name correctly" in {
      bi.name should be (Some("Test Bundle"))
    }
    
    "parse description correctly" in {
      bi.description should be (Some("A description"))
    }
    
    "parse imported packages correctly" in {
      val ip = bi.importedPackages
      ip should contain (ImportDeclaration("junit.runner", false, VersionRange(Version(3, 8, 2)),
          matchingAttributes = Map("matchingAttribute" -> "somevalue")))
      ip should contain (ImportDeclaration("javax.mail", false, VersionRange(Version(1, 4), Version(1, 5), true, false)))
      ip should contain (ImportDeclaration("org.apache.commons.logging", true, VersionRange(Version(1, 0, 4)),
          Some("org.apache.commons"), VersionRange(Version(2, 3, 4))))
    }
    
    "parse exported packages correctly" in {
      val ep = bi.exportedPackages
      ep should contain (ExportDeclaration("de.undercouch.osgiutils", uses = Set(
        "scala.collection.immutable", "scala", "org.scalatest.matchers", "org.scalatest")))
      ep should contain (ExportDeclaration("com.package.with.ver", Version(1, 2, 3)))
      ep should contain (ExportDeclaration("com.package.with.included", includedClasses = Set("com.package.with.included.TestClass")))
      ep should contain (ExportDeclaration("com.package.with.excluded", excludedClasses = Set("com.package.with.excluded.TestClass")))
      ep should contain (ExportDeclaration("com.package.with.mand", mandatoryAttributes = Set("first", "second")))
      ep should contain (ExportDeclaration("com.package.with.all", Version(1, 2, 3), Set("scala", "de.undercouch.osgiutils"),
        Set("first", "second"),
        Set("com.package.with.all.TestClass", "com.package.with.all.TestClass2"),
        Set("com.package.with.all.TestClass", "com.package.with.all.TestClass2"),
        Map("matchingAttribute1" -> "value1", "matchingAttribute2" -> "value2")))
    }
    
    "parse fragment host correctly" in {
      bi.fragmentHost should be (Some(FragmentHost("de.undercouch.scalahelpers", VersionRange(Version(1, 2, 3)))))
    }
    
    "parse required bundles correctly" in {
      val rb = bi.requiredBundles
      rb should contain (RequiredBundle("org.junit", false, VersionRange(Version(3, 8, 2))))
      rb should contain (RequiredBundle("org.apache.ant", true, VersionRange(Version(1, 7 ,1))))
      rb should contain (RequiredBundle("org.apache.log4j", reexport = true))
    }
  }
  
  "BundleInfoFactory" should {
    "complain about invalid bundle" in {
      evaluating {
        BundleInfoFactory.createBundleInfo(getClass.getResource("MANIFEST_INVALID.MF"))
      } should produce [InvalidBundleException]
    }
    
    "return default manifest version number" in {
      val bi = BundleInfoFactory.createBundleInfo(getClass.getResource("MANIFEST_NOMANIFESTVERSION.MF"))
      bi.manifestVersion should be (1)
    }
    
    "return default version number" in {
      val bi = BundleInfoFactory.createBundleInfo(getClass.getResource("MANIFEST_NOVERSION.MF"))
      bi.version should be (Version.Default)
    }
    
    "return no human readable name" in {
      val bi = BundleInfoFactory.createBundleInfo(getClass.getResource("MANIFEST_NONAME_NODESC.MF"))
      bi.name should be (None)
    }
    
    "return no description" in {
      val bi = BundleInfoFactory.createBundleInfo(getClass.getResource("MANIFEST_NONAME_NODESC.MF"))
      bi.description should be (None)
    }
    
    "complain about invalid version number" in {
      evaluating {
        BundleInfoFactory.createBundleInfo(getClass.getResource("MANIFEST_INVALIDVERSION.MF"))
      } should produce [InvalidBundleException]
    }
    
    "be able to ignore unknown directive" in {
      BundleInfoFactory.createBundleInfo(getClass.getResource("MANIFEST_UNKNOWN_DIRECTIVE.MF"))
    }
    
    "be able to ignore unknown parameter" in {
      BundleInfoFactory.createBundleInfo(getClass.getResource("MANIFEST_UNKNOWN_PARAMETER.MF"))
    }
    
    "parse system fragment host correctly" in {
      val bi1 = BundleInfoFactory.createBundleInfo(getClass.getResource("MANIFEST_BOOTFRAGMENT.MF"))
      val bi2 = BundleInfoFactory.createBundleInfo(getClass.getResource("MANIFEST_FRAMEWORKFRAGMENT.MF"))
      bi1.fragmentHost should be (Some(FragmentHost("system.bundle", VersionRange(Version(1, 2, 3)), FragmentHost.Extension.BootClassPath)))
      bi2.fragmentHost should be (Some(FragmentHost("system.bundle", VersionRange(Version(1, 2, 3)), FragmentHost.Extension.Framework)))
    }
  }
}