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

import java.util.jar.Manifest
import scala.collection._
import scala.reflect.BeanProperty
import scala.util.Sorting
import de.undercouch.osgiutils._

/**
 * Provides methods for bundle resolving 
 * @author Michel Kraemer
 */
class BundleRegistry {
  import BundleRegistry._
  import FrameworkConstants._
  import ManifestConstants._
  
  /**
   * A cache used during the resolving process to cache transitive
   * dependencies for a given bundle
   */
  private type ResolverCache = mutable.HashMap[BundleInfo, Set[ResolverResult]]
  
  /**
   * A shortcut for MultiMaps
   * @author Michel Kraemer
   */
  private class MultiMap[A, B] extends mutable.HashMap[A, mutable.Set[B]] with mutable.MultiMap[A, B]
  
  /**
   * An item in the index of exported packages
   * @author Michel Kraemer
   */
  private case class ExportedPackageItem(pkg: ExportedPackage, bundle: BundleInfo)
  
  /**
   * The registered bundles, their ID and the current state (resolved or not)
   */
  private var bundles = Map[BundleInfo, Tuple2[Int, ResolverResult]]()
  
  /**
   * The index of bundles. Maps symbolic names to bundles.
   */
  private val symbolicNameIndex = new MultiMap[String, BundleInfo]()
  
  /**
   * The index of exported packages. Maps package names to
   * exported packages and respective bundles
   */
  private val exportedPackageIndex = new MultiMap[String, ExportedPackageItem]()
  
  /**
   * The index of bundle fragments. Maps symbolic names of
   * host bundles to fragments
   */
  private val fragmentIndex = new MultiMap[String, BundleInfo]()
  
  //add system.bundle. It must be added before any other
  //bundle since the system bundle has always ID 0
  addSystemBundle()
  
  private def addSystemBundle() {
    //get system packages
    var systemPackages = System.getProperty(FrameworkSystemPackages)
    if (systemPackages == null) systemPackages = ""
    var systemPackagesExtra = System.getProperty(FrameworkSystemPackagesExtra)
    if (systemPackagesExtra != null) {
      if (systemPackages.isEmpty) {
        systemPackages = systemPackagesExtra
      } else {
        systemPackages += "," + systemPackagesExtra
      }
    }
    
    //create manifest for system bundle
    val m = new Manifest()
    val attrs = m.getMainAttributes()
    attrs.putValue(BundleSymbolicName, SystemBundleSymbolicName)
    attrs.putValue(ExportPackage, systemPackages)
    
    add(BundleInfo.fromManifest(m))
  }
  
  /**
   * Returns the current state (resolved or unresolved) of the
   * given bundle from the registry
   * @param bundle the bundle
   * @return the current state of the bundle or None if there
   * is no such bundle in the registry
   */
  private def getResolverResult(bundle: BundleInfo) =
    bundles.get(bundle) map { _._2 }
  
  /**
   * Returns the ID of the given bundle. IDs are assigned
   * consecutively to bundles in the order they are added
   * to the registry.
   * @param bundle the bundle
   * @return the bundle's ID or None if there is no such
   * bundle in the registry
   */
  def getId(bundle: BundleInfo) =
    bundles.get(bundle) map { _._1 }
  
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
    bundles += (bundle -> (bundles.size, Unresolved(bundle)))
    
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
   * @return the errors occurred during the resolving process. This
   * set is empty if all bundles were resolved successfully.
   */
  def resolveBundles(): Set[ResolverError] = {
    implicit val cache = new ResolverCache()
    bundles.foldLeft(Set.empty[ResolverError]) { (r, b) => r ++ resolveBundleCached(b._1) }
  }
  
  /**
   * Checks if a bundle is resolved.
   * @param bundle the bundle
   * @return true if the bundle is known by the registry and
   * if it is resolved, false otherwise
   */
  def isResolved(bundle: BundleInfo): Boolean = getResolverResult(bundle) match {
    case Some(Resolved(_)) => true
    case _ => false
  }
  
  /**
   * Trys to resolve a single bundle (no matter if it has
   * already been added to the registry or not)
   * @param bundle the bundle to resolve
   * @return the errors occured during the resolving process. This
   * set is empty if the bundle was resolved successfully.
   */
  def resolveBundle(bundle: BundleInfo): Set[ResolverError] =
    resolveBundleCached(bundle)(new ResolverCache())
    
  private def resolveBundleCached(bundle: BundleInfo)(implicit cache: ResolverCache): Set[ResolverError] = {
    getResolverResult(bundle) match {
      case Some(r: Resolved) => Set.empty
      case _ => resolveBundleInternal(bundle)
    }
  }
  
  private def resolveBundleInternal(bundle: BundleInfo)(implicit cache: ResolverCache): Set[ResolverError] = {
    //find resolver errors
    val errors = calculateRequiredBundlesInternal(bundle, false, List.empty) collect {
      case d: ResolverError => d: ResolverError
    }
    
    //check if there are missing dependencies
    val r = if (errors.isEmpty) Resolved(bundle) else Unresolved(bundle)
    
    //update registry
    bundles.get(bundle) match {
      case Some(current) => bundles += (bundle -> (current._1, r))
      case _ => //do not add new bundles here 
    }
    
    errors
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
    calculateRequiredBundlesInternal(bundle, includeOptional, List.empty)(new ResolverCache())    
  
  private def calculateRequiredBundlesInternal(bundle: BundleInfo, includeOptional: Boolean,
    path: List[BundleInfo])(implicit cache: ResolverCache): Set[ResolverResult] = {
    //check cache first
    cache.get(bundle) match {
      case Some(deps) =>
        //return cached transitive dependencies
        deps
      
      case None =>
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
        
        val r = deps ++ transitive
        
        //cache transitive dependencies
        cache += (bundle -> r)
        
        r
    }
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
    (calculateRequiredBundlesShallowInternal(bundle, includeOptional) flatMap {
      case RequiredBundleWire(_, rb, candidates) => candidates match {
        case List() if rb.optional => None
        case List() => Some(MissingRequiredBundle(bundle, rb))
        case _ => getResolverResult(candidates(0))
      }
      
      case ImportedPackageWire(_, ip, candidates) => candidates match {
        case head :: Nil if head == bundle =>
          //ignore internal dependency, but
          //do not produce an error
          None
        case head :: tail if head == bundle =>
          //ignore internal dependency and
          //use the external one
          getResolverResult(tail(0))
        case head :: tail =>
          //use dependency with highest priority
          getResolverResult(head)
        case List() if ip.optional =>
          //ignore missing dependency if the
          //import-package declaration is optional
          None
        case List() =>
          //produce missing dependency error
          Some(MissingImportedPackage(bundle, ip))
      }
      
      case FragmentHostWire(_, fh, candidates) => candidates match {
        case List() => Some(MissingFragmentHost(bundle, fh))
        case _ => getResolverResult(candidates(0))
      }
    }).toSet
  }
  
  def calculateRequiredBundlesShallowInternal(bundle: BundleInfo, includeOptional: Boolean = false): List[Wire] = {
    //calculate list of required bundles but do not include optional bundles
    //if includeOptional is false or if they are unknown to the registry
    val a = bundle.requiredBundles filter { includeOptional || !_.optional } map { rb =>
      RequiredBundleWire(bundle, rb, findBundles(rb))
    }
    
    //calculate list of required bundles by imported packages
    val b = bundle.importedPackages filter { includeOptional || !_.optional } map { ip =>
      ImportedPackageWire(bundle, ip, findBundles(ip))
    }
    
    //add fragment host as required bundle if there is any
    val c = bundle.fragmentHost map { fh =>
      FragmentHostWire(bundle, fh, findBundles(fh))
    }
    
    a ++ b ++ c
  }
  
  /**
   * Convenience method to find a unique bundle in the registry that
   * matches the given constraints. If the method finds multiple bundles
   * matching the constraints, it returns the one that fits best.
   * @param symbolicName the symbolicName of the bundle to find
   * @param version the version range the returned bundle should match
   * @return the bundle that matches the constraints or None if
   * there is no such bundle in the registry
   */
  def findBundle(symbolicName: String, version: VersionRange): Option[BundleInfo] = {
    val result = findBundles(symbolicName, version)
    if (result.isEmpty) None else Some(result(0))
  }
  
  /**
   * Finds all bundles in the registry that match the given constraints.
   * The returned list of bundles is sorted by their priority. That means
   * the first bundle in the list is the one that matches the constraints best.
   * @param symbolicName the symbolicName of the bundles to find
   * @param version the version range the returned bundles should match
   * @return the bundles that match the constraints or an empty list if
   * there are no such bundles in the registry
   */
  def findBundles(symbolicName: String, version: VersionRange): List[BundleInfo] = {
    symbolicNameIndex.get(symbolicName) match {
      case None => List.empty
      case Some(candidates) => prioritize(candidates filter { version contains _.version })
    }
  }
  
  /**
   * Convenience method to find a unique bundle in the registry that
   * matches the given fragment-host constraint. If the method finds
   * multiple bundles matching the constraint, it returns the one
   * that fits best.
   * @param fh the fragment-host constraint
   * @return the bundle that matches the constraint or None if
   * there is no such bundle in the registry
   */
  def findBundle(fh: FragmentHost): Option[BundleInfo] =
    findBundle(fh.symbolicName, fh.version)
  
  /**
   * Finds all bundles in the registry that match the given
   * fragment-host constraint. The returned list of bundles is sorted
   * by their priority. That means the first bundle in the list is the
   * one that matches the constraint best.
   * @return the bundles that match the constraint or an empty list if
   * there are no such bundles in the registry
   */
  def findBundles(fh: FragmentHost): List[BundleInfo] = 
    findBundles(fh.symbolicName, fh.version)
  
  /**
   * Convenience method to find a unique bundle in the registry
   * that matches the given require-bundle constraint. If the method finds
   * multiple bundles matching the constraint, it returns the one
   * that fits best.
   * @param r the require-bundle constraint
   * @return the bundle that matches the constraint or None if
   * there is no such bundle in the registry
   */
  def findBundle(r: RequiredBundle): Option[BundleInfo] =
    findBundle(r.symbolicName, r.version)
  
  /**
   * Finds all bundles in the registry that match the given
   * require-bundle constraint. The returned list of bundles is
   * sorted by their priority. That means the first bundle in the
   * list is the one that matches the constraint best.
   * @param r the require-bundle constraint
   * @return the bundles that match the constraint or an empty list if
   * there are no such bundles in the registry
   */
  def findBundles(r: RequiredBundle): List[BundleInfo] = 
    findBundles(r.symbolicName, r.version)
  
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
   * Convenience method to find a unique bundle in the registry
   * that exports a package that matches the given import-package
   * constraint. If the method finds multiple bundles exporting such
   * a package, it returns the one that fits best.
   * @param r the import-package constraint
   * @return the bundle that exports a package that matches the
   * constraint or None if there is no such bundle in the registry
   */
  def findBundle(i: ImportedPackage): Option[BundleInfo] = {
    val result = findBundles(i)
    if (result.isEmpty) None else Some(result(0))
  }
  
  /**
   * Finds all bundles in the registry that match the given
   * import-package constraint. The returned list of bundles is
   * sorted by their priority. That means the first bundle in the
   * list is the one that matches the constraint best.
   * @param r the import-package constraint
   * @return the bundles that match the constraint or an empty list if
   * there are no such bundles in the registry
   */
  def findBundles(i: ImportedPackage): List[BundleInfo] = {
    val result = exportedPackageIndex.get(i.name) map { candidates =>
      candidates filter { c =>
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
      } map {
        //convert list of ExportPackage items to bundles
        _.bundle
      }
    }
    prioritize(result getOrElse Set.empty)
  }
  
  /**
   * Prioritizes bundles
   * @param bundles a list of bundles
   * @return the bundles sorted by their priority. The bundle with
   * the highest priority is at position 0.
   */
  private def prioritize(bundles: Iterable[BundleInfo]): List[BundleInfo] = {
    val arr = bundles.toArray
    Sorting.quickSort(arr)(Ordering fromLessThan { (a: BundleInfo, b: BundleInfo) =>
      val ar = isResolved(a)
      val br = isResolved(b)
      if (ar && !br) {
        true
      } else if (ar == br) {
        if (a.version > b.version) {
          true
        } else if (a.version == b.version) {
          val aid = getId(a)
          val bid = getId(b)
          if (aid.isDefined && !bid.isDefined) {
            true
          } else if (!aid.isDefined && bid.isDefined) {
            false
          } else if (!aid.isDefined && !bid.isDefined) {
            false
          } else {
            aid.get < bid.get
          }
        } else {
          false
        }
      } else {
        false
      }
    })
    arr.toList
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
  sealed trait ResolverError
  
  /**
   * The result of the resolving process
   */
  sealed abstract class ResolverResult(@BeanProperty val bundle: BundleInfo)
  
  /**
   * Describes an unresolved bundle.
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
    @BeanProperty val requiredBundle: RequiredBundle) extends ResolverResult(b) with ResolverError {
    override def toString(): String = "Missing required bundle " + requiredBundle + " in " + b
  }
  
  /**
   * This resolver error occurs when an imported package could not be found
   */
  case class MissingImportedPackage(b: BundleInfo,
    @BeanProperty val importedPackage: ImportedPackage) extends ResolverResult(b) with ResolverError {
    override def toString(): String = "Missing imported package " + importedPackage + " in " + b
  }
  
  /**
   * This resolver error occurs when the host of a fragment could not be found
   */
  case class MissingFragmentHost(b: BundleInfo,
    @BeanProperty val fragmentHost: FragmentHost) extends ResolverResult(b) with ResolverError {
    override def toString(): String = "Missing fragment host " + fragmentHost + " in " + b
  }
  
  private sealed abstract class Wire(val bundle: BundleInfo)
  
  private case class RequiredBundleWire(b: BundleInfo, rb: RequiredBundle,
    candidates: List[BundleInfo]) extends Wire(b)
  
  private case class ImportedPackageWire(b: BundleInfo, ip: ImportedPackage,
    candidates: List[BundleInfo]) extends Wire(b)
  
  private case class FragmentHostWire(b: BundleInfo, fh: FragmentHost,
    candidates: List[BundleInfo]) extends Wire(b)
}
