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
   * An item in the index of exported packages
   * @author Michel Kraemer
   */
  case class ExportedPackageItem(pkg: ExportedPackage, bundle: BundleInfo)
  
  /**
   * The registered bundles
   */
  var bundles = mutable.LinkedHashMap[BundleInfo, ResolverResult]()
  
  /**
   * The index of bundles. Maps symbolic names to bundles.
   */
  val symbolicNameIndex = new mutable.LinkedHashMap[String, mutable.Set[BundleInfo]]()
    with mutable.MultiMap[String, BundleInfo] {
    override protected def makeSet: mutable.Set[BundleInfo] =
      new mutable.LinkedHashSet[BundleInfo]
  }
  
  /**
   * The index of exported packages. Maps package names to
   * exported packages and respective bundles
   */
  val exportedPackageIndex = new mutable.LinkedHashMap[String, mutable.Set[ExportedPackageItem]]()
    with mutable.MultiMap[String, ExportedPackageItem] {
    override protected def makeSet: mutable.Set[ExportedPackageItem] =
      new mutable.LinkedHashSet[ExportedPackageItem]
  }
  
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
  }
  
  /**
   * Trys to resolve all bundles not yet resolved
   * @return a list of resolver results
   */
  def resolveBundles(): Seq[ResolverResult] = {
    bundles foreach { rr => if (!rr._2.resolved) resolveBundle(rr._1) }
    bundles.values.toSeq
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
    //TODO optional bundles should not provide resolving
    var result = (bundle.requiredBundles flatMap findBundle).toSet
    //if (rb forall isResolved) ResolverResult(bundle, true) else ResolverResult(bundle, false)
    ResolverResult(bundle, false)
    
    //TODO put resolved bundle back into map of bundles
  }
  
  /**
   * Finds a unique bundle in the registry that matches the
   * given require-bundle constraint
   * @param r the require-bundle constraint
   * @return the bundle that matches the constraint or None if
   * there is no such bundle in the registry
   */
  def findBundle(r: RequiredBundle): Option[BundleInfo] = {
    symbolicNameIndex.get(r.symbolicName) flatMap { candidates =>
      val r = candidates filter { bundle =>
        true
      }
      if (r.isEmpty) None else Some(r.iterator.next)
    }
  }
}
