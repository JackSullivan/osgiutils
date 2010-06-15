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

import java.util.jar.{Attributes, Manifest}
import scala.reflect.BeanProperty
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input._

/**
 * Provides information about an OSGi bundle
 * @author Michel Kraemer
 */
case class BundleInfo(
  /**
   * The bundle's manifest
   */
  @BeanProperty
  val manifest: Manifest,
  
  /**
   * The version of the OSGi specification.
   * 1 mean R3, 2 means R4 and later.
   */
  @BeanProperty
  val manifestVersion: Int,
  
  /**
   * The bundle's symbolic name
   */
  @BeanProperty
  val symbolicName: String,
  
  /**
   * A human readable name
   */
  @BeanProperty
  val name: Option[String],
  
  /**
   * The bundle's description
   */
  @BeanProperty
  val description: Option[String],
  
  /**
   * The bundle's version
   */
  @BeanProperty
  val version: BundleInfo.Version,
  
  //TODO
  //val exportedPackages
  
  //TODO
  //val fragmentHost
  
  /**
   * The packages imported by this bundle
   */
  val importedPackages: Array[BundleInfo.ImportDeclaration]
  
  //TODO
  //val requiredBundles
) {
  def this(manifest: Manifest) = {
    this(
        manifest,
        BundleInfo.parseManifestVersion(manifest),
        BundleInfo.parseSymbolicName(manifest),
        BundleInfo.getSimpleManifestEntry(manifest, ManifestConstants.BundleName),
        BundleInfo.getSimpleManifestEntry(manifest, ManifestConstants.BundleDescription),
        BundleInfo.parseVersion(manifest),
        BundleInfo.parseImportedPackages(manifest)
    )
  }
  
  def getManifestEntry(name: String): BundleInfo.Header =
    BundleInfo.parseManifestEntry(manifest, name)
  
  def getSimpleManifestEntry(name: String): Option[String] =
    BundleInfo.getSimpleManifestEntry(manifest, name)
}

/**
 * Provides methods to parse bundle manifests
 * @author Michel Kraemer
 */
object BundleInfo {
  type HeaderClause = Array[String]
  type Header = Array[HeaderClause]
  
  private val attrPattern = """\s*=\s*(.+)"""
  private val ResolutionRegex = ("resolution" + attrPattern)r
  private val VersionRegex = ("version" + attrPattern)r
  private val SpecificationVersionRegex = ("specification-version" + attrPattern)r
  private val BundleSymbolicNameRegex = ("bundle-symbolic-name" + attrPattern)r
  private val BundleVersionRegex = ("bundle-version" + attrPattern)r
  
  case class Version(major: Int, minor: Int, micro: Int, qualifier: Option[String])
  
  object Version {
    val Default = Version(0, 0, 0, None)
    
    val Infinite = new Version(0, 0, 0, None) {
      //TODO overwrite comparison operators
    }
    
    def apply(major: Int): Version =
      Version(major, 0, 0, None)
      
    def apply(major: Int, minor: Int): Version =
      Version(major, minor, 0, None)
      
    def apply(major: Int, minor: Int, micro: Int): Version =
      Version(major, minor, micro, None)
    
    def apply(major: Int, minor: Int, micro: Int, qualifier: String): Version =
      Version(major, minor, micro, if (qualifier != null) Some(qualifier) else None)
      
    def apply(v: String): Version = {
      val n = v.split("\\.")
      try {
        Version(
            if (n.length > 0) n(0).toInt else 0,
            if (n.length > 1) n(1).toInt else 0,
            if (n.length > 2) n(2).toInt else 0,
            if (n.length > 3) Some(n(3)) else None
        )
      } catch {
        case _: NumberFormatException =>
          throw new InvalidBundleException("Invalid version number: " + v)
      }
    }
  }
  
  case class VersionRange(floor: Version, ceiling: Version, floorInclusive: Boolean, ceilingInclusive: Boolean)
  
  object VersionRange {
    val Default = VersionRange(Version.Default)
    
    def apply(v: String): VersionRange = {
      object RangeParser extends RegexParsers {
        lazy val range = interval | atleast
        lazy val interval = ( floorinc | floorexc ) ~ version ~ "," ~ version ~ ( ceilinc | ceilexc ) ^^ {
          case left ~ floor ~ "," ~ ceiling ~ right => VersionRange(floor, ceiling, left, right)
        }
        lazy val floorinc = "[" ^^^ true
        lazy val floorexc = "(" ^^^ false
        lazy val ceilinc = "]" ^^^ true
        lazy val ceilexc = ")" ^^^ false
        lazy val atleast = version ^^ { VersionRange(_, Version.Infinite, true, false) }
        lazy val version = regex("[0-9\\.a-zA-Z_-]+"r) ^^ Version.apply
      }
      
      RangeParser.range(new CharSequenceReader(v)) match {
        case RangeParser.Success(result, next) if next.atEnd => result
        case s => throw new InvalidBundleException("Invalid version range: " + v)
      }
    }
    
    def apply(v: Version): VersionRange =
      VersionRange(v, Version.Infinite, true, false)
  }
  
  case class ImportDeclaration(name: String, version: VersionRange)
  
  private def parseManifestEntry(manifest: Manifest, name: String): Header = {
    val v = getSimpleManifestEntry(manifest, name)
    if (v.isDefined) parseManifestEntry(v.get) else Array.empty
  }
  
  private def getSimpleManifestEntry(manifest: Manifest, name: String): Option[String] = {
    val attrs = manifest.getMainAttributes()
    if (attrs == null) None else getSimpleManifestEntry(attrs, name)
  }
  
  private def getSimpleManifestEntry(attrs: Attributes, name: String): Option[String] = {
    val v = attrs.getValue(name)
    if (v == null) None else Some(v)
  }
  
  //TODO split by "," is bad since version ranges also contain commas
  //TODO a custom split method is needed which pays attention to quotation marks
  private def parseManifestEntry(v: String): Header =
    v.trim.split(",") map (v => (v.trim.split(";")) map (_.trim))
  
  private def parseManifestVersion(manifest: Manifest): Int = {
    val oh = getSimpleManifestEntry(manifest, ManifestConstants.BundleManifestVersion)
    try {
      oh map (_.toInt) getOrElse 1
    } catch {
      case _: NumberFormatException =>
        throw new InvalidBundleException("Invalid manifest version: " + oh.get)
    }
  }
  
  private def parseSymbolicName(manifest: Manifest): String = {
    val h = parseManifestEntry(manifest, ManifestConstants.BundleSymbolicName)
    if (h.isEmpty || h(0).isEmpty) {
      throw new InvalidBundleException("Manifest contains no symbolic name")
    }
    h(0)(0)
  }
  
  private def parseVersion(manifest: Manifest): Version = {
    val oh = getSimpleManifestEntry(manifest, ManifestConstants.BundleVersion)
    oh map Version.apply getOrElse Version.Default
  }
  
  //TODO could be added to the regex or could be handled by a parser
  private def trimQuotes(s: String): String = {
    var r = s.trim
    if (r.startsWith("\"")) r = r.substring(1)
    if (r.endsWith("\"")) r = r.substring(0, r.length - 1)
    r
  }
  
  private def parseImportedPackages(manifest: Manifest): Array[ImportDeclaration] = {
    var allNames = Set[String]()
    
    //TODO could be replaced by a parser
    def parsePackageDeclaration(decl: HeaderClause): Set[ImportDeclaration] = {
      var names = Set[String]()
      var optional = false
      var version = VersionRange.Default
      for (param <- decl) param match {
        case ResolutionRegex(v) =>
          optional = parseResolution(v)
          
        case VersionRegex(v) =>
          version = VersionRange(trimQuotes(v))
          
        case SpecificationVersionRegex(v) => //TODO
        
        case BundleSymbolicNameRegex(v) => //TODO
        
        case BundleVersionRegex => //TODO
          
        case s =>
          if (allNames contains s) {
            throw new InvalidBundleException("Duplicate import package: " + s)
          } else {
            allNames += s
            names += s
          }
      }
      
      for (n <- names) yield ImportDeclaration(n, version)
    }
    
    val h = parseManifestEntry(manifest, ManifestConstants.ImportPackage)
    h flatMap parsePackageDeclaration
  }
  
  private def parseResolution(v: String) = v match {
    case "optional" => true
    case "mandatory" => false
    case _ => throw new InvalidBundleException("Invalid value for resolution attribute" + v)
  }
}
