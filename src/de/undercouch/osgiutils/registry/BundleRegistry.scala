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

import scala.collection._
import de.undercouch.osgiutils._

/**
 * Provides methods for bundle resolving 
 * @author Michel Kraemer
 */
class BundleRegistry {
  /**
   * A MultiMap which keeps the order of inserted elements. This is necessary since
   * there is a prioritization of registered bundles: bundles with a lower ID (i.e.
   * which have been added first) should be prefered during resolving.
   * @author Michel Kraemer
   */
  private class LinkedMultiMap[A, B] extends mutable.LinkedHashMap[A, mutable.Set[B]]
    with mutable.MultiMap[A, B] {
    override protected def makeSet: mutable.Set[B] = new mutable.LinkedHashSet[B]
  }
  
  /**
   * An item in the index of exported packages
   * @author Michel Kraemer
   */
  private case class ExportedPackageItem(pkg: ExportedPackage, bundle: BundleInfo)
  
  /**
   * The registered bundles
   */
  private var bundles = mutable.LinkedHashMap[BundleInfo, ResolverResult]()
  
  /**
   * The index of bundles. Maps symbolic names to bundles.
   */
  private val symbolicNameIndex = new LinkedMultiMap[String, BundleInfo]()
  
  /**
   * The index of exported packages. Maps package names to
   * exported packages and respective bundles
   */
  private val exportedPackageIndex = new LinkedMultiMap[String, ExportedPackageItem]()
  
  /**
   * The index of bundle fragments. Maps symbolic names of
   * host bundles to fragments
   */
  private val fragmentIndex = new LinkedMultiMap[String, BundleInfo]()
  
  /**
   * Adds a bundle to the registry
   * @param bundle the bundle to add
   * @throws IllegalStateException if the given bundle has
   * already been added to the registry
   */
  def add(bundle: BundleInfo) {
    if (bundles contains bundle)
      throw new IllegalStateException("Bundle has already been added to the registry")
    
    //add bundle to registry
    bundles += (bundle -> ResolverResult(bundle))
    
    //add bundle to index
    symbolicNameIndex.addBinding(bundle.symbolicName, bundle)
    
    //add exported packages to index
    bundle.exportedPackages foreach { ep => exportedPackageIndex.addBinding(ep.name,
      ExportedPackageItem(ep, bundle)) }
    
    //add fragment to index
    for (host <- bundle.fragmentHost) fragmentIndex.addBinding(host.symbolicName, bundle)
  }
  
  /**
   * Trys to resolve all bundles not yet resolved
   * @return a list of resolver results
   */
  def resolveBundles(): Iterable[ResolverResult] = {
    for (b <- bundles if !b._2.resolved) resolveBundle(b._1)
    bundles.values
  }
  
  /**
   * Checks if a bundle is resolved.
   * @param bundle the bundle
   * @return true if the bundle is known by the registry and
   * if it is resolved, false otherwise
   */
  def isResolved(bundle: BundleInfo): Boolean =
    (for (b <- bundles.get(bundle)) yield b.resolved) getOrElse false
  
  /**
   * Trys to resolve a single bundle (no matter if it has
   * already been added to the registry or not)
   * @param bundle the bundle to resolve
   * @return the resolver result
   */
  def resolveBundle(bundle: BundleInfo): ResolverResult = {
    //TODO
    //val rb = calculateRequiredBundles(bundle)
    //find required bundles
    //TODO optional bundles should not prevent resolving
    var result = (bundle.requiredBundles flatMap { r => findBundle(r) }).toSet
    //if (rb forall isResolved) ResolverResult(bundle, true) else ResolverResult(bundle, false)
    ResolverResult(bundle, false)
    
    //TODO put resolved bundle back into map of bundles
  }
  
  /**
   * Finds a unique bundle in the registry that matches the
   * given constraints
   * @param symbolicName the symbolicName of the bundle to find
   * @param version the version range the returned bundle should match
   * @return the bundle that matches the constraints or None if
   * there is no such bundle in the registry
   */
  def findBundle(symbolicName: String, version: VersionRange): Option[BundleInfo] = {
    symbolicNameIndex.get(symbolicName) flatMap { candidates =>
      val result = candidates filter { version contains _.version }
      if (result.isEmpty) None else Some(result.iterator.next)
    }
  }
  
  /**
   * Finds a unique bundle in the registry that matches the
   * given fragment-host constraint
   * @param fh the fragment-host constraint
   * @return the bundle that matches the constraint or None if
   * there is no such bundle in the registry
   */
  def findBundle(fh: FragmentHost): Option[BundleInfo] =
    findBundle(fh.symbolicName, fh.version)
  
  /**
   * Finds a unique bundle in the registry that matches the
   * given require-bundle constraint
   * @param r the require-bundle constraint
   * @return the bundle that matches the constraint or None if
   * there is no such bundle in the registry
   */
  def findBundle(r: RequiredBundle): Option[BundleInfo] =
    findBundle(r.symbolicName, r.version)
  
  /**
   * Finds all fragments of a given bundle
   * @param bundle the fragment host
   * @return the list of fragments for the given bundle or
   * an empty list if there are no fragment for this bundle
   * in the registry or if the bundle itself is not known by the registry
   */
  def findFragments(bundle: BundleInfo): Iterable[BundleInfo] = {
    fragmentIndex.get(bundle.symbolicName) map { candidates =>
      candidates filter { _.fragmentHost.get.version contains bundle.version }
    } getOrElse mutable.Set.empty[BundleInfo]
  }
  
  /**
   * Finds a unique bundle in the registry that exports a package
   * that matches the given import-package constraint
   * @param r the import-package constraint
   * @return the bundle that exports a package that matches the
   * constraint or None if there is no such bundle in the registry
   */
  def findBundle(i: ImportedPackage): Option[BundleInfo] = {
    exportedPackageIndex.get(i.name) flatMap { candidates =>
      val result = candidates filter { c =>
        //check if the version of the exported package matches the required one
        i.version contains c.pkg.version
      } filter { c =>
        //check if there is a required bundle symbolic name
        //if so, filter out candidates that do not match
        (for (sn <- i.bundleSymbolicName) yield c.bundle.symbolicName == sn) getOrElse true
      } filter { c =>
        //filter out candidates that do not match the required bundle version
        i.bundleVersion contains c.bundle.version
      } filter { c =>
        //filter out candidates that require mandatory attributes
        //the imported-package declaration does not specify
        c.pkg.mandatoryAttributes forall i.matchingAttributes.contains
      } filter { c =>
        //check attributes and filter out candidates that do not match
        i.matchingAttributes forall { ia =>
          (for (ca <- c.pkg.matchingAttributes.get(ia._1)) yield ia._2 == ca) getOrElse false
        }
      }
      if (result.isEmpty) None else Some(result.iterator.next.bundle)
    }
  }
}
