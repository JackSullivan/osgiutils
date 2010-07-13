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
  
  /**
   * Creates the system bundle and adds it to the registry. Uses the
   * system properties <code>org.osgi.framework.system.packages</code> and
   * <code>org.osgi.framework.system.packages.extra</code> to build to list
   * of packages exported by the system bundle.
   */
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
    implicit val tcache = new TraversalCache()
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
   * @return the errors occurred during the resolving process. This
   * set is empty if the bundle was resolved successfully.
   */
  def resolveBundle(bundle: BundleInfo): Set[ResolverError] =
    resolveBundleCached(bundle)(new ResolverCache(), new TraversalCache())
  
  /**
   * Trys to resolve a single bundle (no matter if it has
   * already been added to the registry or not). Caches transitive dependencies
   * to speed up resolving process.
   * @param bundle the bundle to resolve
   * @param cache saves transitives dependencies during the resolving process 
   * @return the errors occurred during the resolving process. This
   * set is empty if the bundle was resolved successfully.
   */
  private def resolveBundleCached(bundle: BundleInfo)(implicit cache: ResolverCache,
      tcache: TraversalCache): Set[ResolverError] = {
    getResolverResult(bundle) match {
      case Some(r: Resolved) => Set.empty
      case _ => resolveBundleInternal(bundle)
    }
  }
  
  private def resolveBundleInternal(bundle: BundleInfo)(implicit cache: ResolverCache,
      tcache: TraversalCache): Set[ResolverError] = {
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
    calculateRequiredBundlesInternal(bundle, includeOptional, List.empty)(new ResolverCache(), new TraversalCache())
  
  private def calculateRequiredBundlesInternal(bundle: BundleInfo, includeOptional: Boolean,
      path: List[BundleInfo])(implicit cache: ResolverCache, tcache: TraversalCache): Set[ResolverResult] =
    traverseGraph(calculateGraph(bundle, includeOptional, path))
  
  /**
   * Calculates the transitive dependency graph of the given bundle. This
   * method returns a BundleNode that contains a list of Wires that
   * respresent the bundle's dependencies.
   * @param bundle the bundle to calculate the transitive dependencies for
   * @param includeOptional true if optional dependencies should also be
   * calculated if they are known to the registry
   * @param cache saves transitives dependencies during the resolving process 
   * @return a BundleNode object
   */
  private def calculateGraph(bundle: BundleInfo, includeOptional: Boolean,
    path: List[BundleInfo])(implicit cache: ResolverCache): BundleNode = {
    //check cache first
    cache.get(bundle) match {
      case Some(node) =>
        //return cached transitive dependencies
        node
      
      case None =>
        //calculate direct dependencies for the current bundle
        val deps = calculateWiresShallow(bundle, includeOptional)
        
        //calculate transitive dependencies
        val transitive: Set[Wire] = deps map {
          case w: DirectWire =>
            DirectWire(calculateWiresFor(w.candidate, bundle, includeOptional, path))
            
          case w: RequiredBundleWire =>
            RequiredBundleWire(w.rb, w.candidates map { c =>
              calculateWiresFor(c, bundle, includeOptional, path)
            })
          
          case w: ImportedPackageWire =>
            ImportedPackageWire(w.p, w.candidates map { c =>
              calculateWiresFor(c, bundle, includeOptional, path)
            })
          
          case w: FragmentHostWire =>
            FragmentHostWire(w.fh, w.candidates map { c =>
              calculateWiresFor(c, bundle, includeOptional, path)
            })
        }
        
        val node = BundleNode(bundle, transitive)
        
        //cache transitive dependencies
        cache += (bundle -> node)
        
        node
    }
  }
  
  private def calculateWiresFor(node: BundleNode, bundle: BundleInfo, includeOptional: Boolean,
      path: List[BundleInfo])(implicit cache: ResolverCache): BundleNode = node match {
    case BundleNode(candidate, _) if candidate == bundle =>
      //ignore internal dependency
      node
      
    case BundleNode(candidate, _) if path.contains(candidate) =>
      //the current bundle depends on a bundle already in the current path
      //that means that a dependency cycle has been detected
      val cycle = (path drop (path indexOf candidate)) ++ List(bundle, candidate)
      val symbolicNames = cycle map { _.symbolicName } reduceLeft { _ + ", " + _ }
      throw new DependencyCycleException("Dependency cycle detected: " + symbolicNames, cycle.toArray)
    
    case BundleNode(candidate, _) =>
      //calculate transitive dependencies
      calculateGraph(candidate, includeOptional, path ++ List(bundle))
  }
  
  /**
   * Calculates the direct, non-transitive dependencies of the given bundle. This
   * method returns a set that contains a Wire object for each dependency. Each Wire
   * contains a list of bundles that fulfill the respective dependency. If this list
   * is empty, the wire describes a missing dependency. 
   * @param bundle the bundle to calculate the direct dependencies for
   * @param includeOptional true if optional dependencies should also be
   * calculated if they are known to the registry
   * @return a set of Wire objects
   */
  private def calculateWiresShallow(bundle: BundleInfo,
      includeOptional: Boolean = false): Set[Wire] = {
    //calculate list of required bundles but do not include optional bundles
    //if includeOptional is false or if they are unknown to the registry
    val a = bundle.requiredBundles filter { includeOptional || !_.optional } map { rb =>
      val bundles = findBundles(rb)
      if (bundles.length == 1)
        DirectWire(BundleNode(bundles(0)))
      else
        RequiredBundleWire(rb, bundles map { b => BundleNode(b) })
    }
    
    //calculate list of required bundles by imported packages
    val b = bundle.importedPackages filter { includeOptional || !_.optional } map { ip =>
      val bundles = findBundles(ip)
      if (bundles.length == 1)
        DirectWire(BundleNode(bundles(0)))
      else
        ImportedPackageWire(ip, bundles map { b => BundleNode(b) })
    }
    
    //add fragment host as required bundle if there is any
    val c = bundle.fragmentHost map { fh =>
      val bundles = findBundles(fh)
      if (bundles.length == 1)
        DirectWire(BundleNode(bundles(0)))
      else
        FragmentHostWire(fh, bundles map { b => BundleNode(b) })
    }
    
    (a ++ b ++ c).toSet
  }
  
  /**
   * Traverses the given dependency graph and finds the best combination of
   * bundles that fulfills all constraints and is free of uses conflicts
   * @param graph the dependency graph to traverse
   * @param cache a cache used to speed up traversal
   * @return a set containing the calculated transitive dependencies
   */
  private def traverseGraph(graph: BundleNode)(implicit cache: TraversalCache): Set[ResolverResult] = {
    //check cache first
    cache.get(graph.bundle) match {
      case Some(s) =>
        //return cached result first
        s
      
      case None =>
        val shallow = traverseWires(graph.wires, graph.bundle)
        val transitive = shallow.foldLeft(Set.empty[ResolverResult]) {
          case (s, Left(n)) => s ++ getResolverResult(n.bundle) ++ traverseGraph(n)
          case (s, Right(e)) => s + e
        }
        
        //cache result (only of it is not ambiguous)
        val ambigious = graph.wires exists {
          case aw: AmbiguousWire if aw.candidates.length == 2 => aw.candidates(0) != graph.bundle
          case aw: AmbiguousWire if aw.candidates.length > 2 => true
          case _ => false
        }
        if (!ambigious)
          cache += (graph.bundle -> transitive)
        
        transitive
    }
  }
  
  /**
   * Traverses the list of wires and returns resolver result objects
   * describing either valid dependencies or constraint errors.
   * @param wires the wires to traverse
   * @param bundle the bundle the wires have been calculated for
   * @return a set of resolver result objects
   */
  private def traverseWires(wires: Set[Wire], bundle: BundleInfo): Set[Either[BundleNode, ResolverResult]] = {
    wires flatMap {
      case DirectWire(candidate) =>
        if (candidate.bundle == bundle)
          //ignore internal dependency
          None
        else
          Some(Left(candidate))
      
      case ImportedPackageWire(ip, candidates) => candidates match {
        case head :: Nil if head.bundle == bundle =>
          //ignore internal dependency, but
          //do not produce an error
          None
        case head :: tail if head.bundle == bundle =>
          //ignore internal dependency and
          //use the external one
          Some(Left(tail(0)))
        case head :: tail =>
          //use dependency with highest priority
          Some(Left(head))
        case List() if ip.optional =>
          //ignore missing dependency if the
          //import-package declaration is optional
          None
        case List() =>
          //produce missing dependency error
          Some(Right(MissingImportedPackage(bundle, ip)))
      }
      
      case RequiredBundleWire(rb, candidates) => candidates match {
        case List() if rb.optional => None
        case List() => Some(Right(MissingRequiredBundle(bundle, rb)))
        case head :: Nil if head.bundle == bundle => None
        case head :: tail if head.bundle == bundle => Some(Left(tail(0)))
        case _ => Some(Left(candidates(0)))
      }
      
      case FragmentHostWire(fh, candidates) => candidates match {
        case List() => Some(Right(MissingFragmentHost(bundle, fh)))
        case head :: Nil if head.bundle == bundle => None
        case head :: tail if head.bundle == bundle => Some(Left(tail(0)))
        case _ => Some(Left(candidates(0)))
      }
    }
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
   * A shortcut for mutable.MultiMap
   * @author Michel Kraemer
   */
  private class MultiMap[A, B] extends mutable.HashMap[A, mutable.Set[B]] with mutable.MultiMap[A, B]
  
  /**
   * An item in the index of exported packages
   * @author Michel Kraemer
   */
  private case class ExportedPackageItem(pkg: ExportedPackage, bundle: BundleInfo)
  
  /**
   * A marker interface for resolver errors
   */
  sealed trait ResolverError
  
  /**
   * The result of the resolving process
   */
  sealed trait ResolverResult {
    @BeanProperty
    val bundle: BundleInfo
  }
  
  /**
   * Describes an unresolved bundle
   */
  case class Unresolved(@BeanProperty bundle: BundleInfo) extends ResolverResult
  
  /**
   * Describes a resolved bundle
   */
  case class Resolved(@BeanProperty bundle: BundleInfo) extends ResolverResult
  
  /**
   * This resolver error occurs when a required bundle could not be found
   */
  case class MissingRequiredBundle(@BeanProperty bundle: BundleInfo,
    @BeanProperty val requiredBundle: RequiredBundle) extends ResolverResult with ResolverError {
    override def toString(): String = "Missing required bundle " + requiredBundle + " in " + bundle
  }
  
  /**
   * This resolver error occurs when an imported package could not be found
   */
  case class MissingImportedPackage(@BeanProperty bundle: BundleInfo,
    @BeanProperty val importedPackage: ImportedPackage) extends ResolverResult with ResolverError {
    override def toString(): String = "Missing imported package " + importedPackage + " in " + bundle
  }
  
  /**
   * This resolver error occurs when the host of a fragment could not be found
   */
  case class MissingFragmentHost(@BeanProperty bundle: BundleInfo,
    @BeanProperty val fragmentHost: FragmentHost) extends ResolverResult with ResolverError {
    override def toString(): String = "Missing fragment host " + fragmentHost + " in " + bundle
  }
  
  /**
   * A cache used during the resolving process to cache transitive
   * dependencies for a given bundle
   */
  private type ResolverCache = mutable.HashMap[BundleInfo, BundleNode]
  
  /**
   * A cache used during traversal of wires
   */
  private type TraversalCache = mutable.HashMap[BundleInfo, Set[ResolverResult]]
  
  /**
   * <p>This is a node in the dependency graph.</p>
   * <p>Overrides <code>equals()</code> and <code>hashCode()</code> since
   * <code>bundle</code> is the only attribute of interest. This speeds up
   * the resolving process a lot.</p>
   */
  private case class BundleNode(val bundle: BundleInfo, val wires: Set[Wire] = Set.empty) {
    override def equals(o: Any): Boolean = o match {
      case that: BundleNode => bundle == that.bundle
      case _ => false
    }
    
    override def hashCode(): Int = bundle.hashCode
  }
  
  /**
   * Describes a relation from a bundle to a list of candidates that
   * would fulfill the bundle's dependencies
   */
  private sealed trait Wire
  
  /**
   * A wire that has only one candidate
   */
  private case class DirectWire(candidate: BundleNode) extends Wire
  
  /**
   * A trait for wires that have more than one candidate
   */
  private trait AmbiguousWire {
    val candidates: List[BundleNode]
  }
  
  /**
   * A wire that contains the candidates that would fulfill a
   * required-bundle constraint
   */
  private case class RequiredBundleWire(rb: RequiredBundle,
    candidates: List[BundleNode]) extends Wire with AmbiguousWire
  
  /**
   * A wire that contains the candidates that would fulfill a
   * imported-package constraint
   */
  private case class ImportedPackageWire(p: ImportedPackage,
    candidates: List[BundleNode]) extends Wire with AmbiguousWire
  
  /**
   * A wire that contains the candidates that would fulfill a
   * fragment-host constraint
   */
  private case class FragmentHostWire(fh: FragmentHost,
    candidates: List[BundleNode]) extends Wire with AmbiguousWire
}
