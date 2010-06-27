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
import scala.reflect.BeanProperty
import de.undercouch.osgiutils._

/**
 * Provides methods for bundle resolving 
 * @author Michel Kraemer
 */
class BundleRegistry {
  import BundleRegistry._
  
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
    bundles += (bundle -> Unresolved(bundle))
    
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
    for (b <- bundles) resolveBundle(b._1)
    bundles.values
  }
  
  /**
   * Checks if a bundle is resolved.
   * @param bundle the bundle
   * @return true if the bundle is known by the registry and
   * if it is resolved, false otherwise
   */
  def isResolved(bundle: BundleInfo): Boolean = bundles.get(bundle) match {
    case Some(Resolved(_)) => true
    case _ => false
  }
  
  /**
   * Trys to resolve a single bundle (no matter if it has
   * already been added to the registry or not)
   * @param bundle the bundle to resolve
   * @return the resolver result
   */
  def resolveBundle(bundle: BundleInfo): ResolverResult = bundles.get(bundle) match {
    case Some(r: Resolved) => r
    case _ => resolveBundleInternal(bundle)
  }
  
  def resolveBundleInternal(bundle: BundleInfo): ResolverResult = {
    //TODO
    //val rb = calculateRequiredBundles(bundle)
    //find required bundles
    //TODO optional bundles should not prevent resolving
    var result = (bundle.requiredBundles flatMap { r => findBundle(r) }).toSet
    //if (rb forall isResolved) ResolverResult(bundle, true) else ResolverResult(bundle, false)
    Unresolved(bundle)
    
    //TODO put resolved bundle back into map of bundles
  }
  
  /**
   * Calculates the transitive dependencies of the given bundle. This
   * method returns a list of ResolverResult objects. Each ResolverResult object describes
   * either a dependency to a bundle known by the registry (no matter if it is
   * resolved or not) or a missing dependency.
   * @param bundle the bundle to calculate the transitive dependencies for
   * @param includeOptional true if optional dependencies should also be
   * calculated if they are known to the registry
   * @return a list of ResolverResult objects that describe either valid
   * dependencies (resolved or unresolved bundles) or missing ones
   * @throws DependencyCycleException if the transitive dependencies contain a cycle
   */
  def calculateRequiredBundles(bundle: BundleInfo, includeOptional: Boolean = false): Set[ResolverResult] =
    calculateRequiredBundlesInternal(bundle, includeOptional, List.empty)    
  
  private def calculateRequiredBundlesInternal(bundle: BundleInfo, includeOptional: Boolean = false,
    path: List[BundleInfo]): Set[ResolverResult] = {
    //calculate direct dependencies for the current bundle
    val deps = calculateRequiredBundlesShallow(bundle, includeOptional)
    
    //calculate transitive dependencies
    val transitive = deps flatMap {
      case _: ResolverError =>
        //don't calculate dependencies for errors
        Nil
      
      case d if path.contains(d.bundle) =>
        //the current bundle depends on a bundle already in the current path
        //that means that a dependency cycle has been detected
        val cycle = (path drop (path indexOf d.bundle)) ++ List(bundle, d.bundle)
        val symbolicNames = cycle map { _.symbolicName } reduceLeft { _ + ", " + _ }
        throw new DependencyCycleException("Dependency cycle detected: " + symbolicNames, cycle.toArray)
        
      case d =>
        //calculate transitive dependencies
        calculateRequiredBundlesInternal(d.bundle, includeOptional, path ++ List(bundle))
    }
    
    deps ++ transitive
  }
  
  /**
   * Calculates the direct, non-transitive dependencies of the given bundle. This
   * method returns a set of ResolverResult objects. Each ResolverResult object describes
   * either a dependency to a bundle known by the registry (no matter if it is
   * resolved or not) or a missing dependency.
   * @param bundle the bundle to calculate the direct dependencies for
   * @param includeOptional true if optional dependencies should also be
   * calculated if they are known to the registry
   * @return a set of ResolverResult objects that describe either valid
   * dependencies (resolved or unresolved bundles) or missing ones
   */
  def calculateRequiredBundlesShallow(bundle: BundleInfo, includeOptional: Boolean = false): Set[ResolverResult] = {
    //calculate list of required bundles but do not include optional bundles
    //if includeOptional is false or if they are unknown to the registry
    val a = bundle.requiredBundles filter { includeOptional || !_.optional } flatMap { rb =>
      findBundle(rb) match {
        case Some(b) => bundles.get(b)
        case None if rb.optional => None
        case None => Some(MissingRequiredBundle(bundle, rb))
      }
    }
    
    //calculate list of required bundles by imported packages
    val b = bundle.importedPackages filter { includeOptional || !_.optional } flatMap { ip =>
      findBundle(ip) match {
        case Some(b) => bundles.get(b)
        case None if ip.optional => None
        case None => Some(MissingImportedPackage(bundle, ip))
      }
    }
    
    //add fragment host as required bundle if there is any
    val c = bundle.fragmentHost map { fh =>
      findBundle(fh) match {
        case Some(b) => bundles.get(b).get
        case None => MissingFragmentHost(bundle, fh)
      }
    }
    
    (a ++ b ++ c).toSet
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

/**
 * Static definitions for the BundleRegistry
 * @author Michel Kraemer
 */
object BundleRegistry {
  /**
   * A marker interface for resolver errors
   */
  trait ResolverError
  
  /**
   * The result of the resolving process
   */
  sealed abstract class ResolverResult(@BeanProperty val bundle: BundleInfo)
  
  /**
   * Describes an unresolved bundle
   */
  case class Unresolved(b: BundleInfo) extends ResolverResult(b)
  
  /**
   * Describes a resolved bundle
   */
  case class Resolved(b: BundleInfo) extends ResolverResult(b)
  
  /**
   * This resolver error occurs when a required bundle could not be found
   */
  case class MissingRequiredBundle(b: BundleInfo,
    @BeanProperty val requiredBundle: RequiredBundle) extends ResolverResult(b) with ResolverError
  
  /**
   * This resolver error occurs when an imported package could not be found
   */
  case class MissingImportedPackage(b: BundleInfo,
    @BeanProperty val importedPackage: ImportedPackage) extends ResolverResult(b) with ResolverError
  
  /**
   * This resolver error occurs when the host of a fragment could not be found
   */
  case class MissingFragmentHost(b: BundleInfo,
    @BeanProperty val fragmentHost: FragmentHost) extends ResolverResult(b) with ResolverError
}
