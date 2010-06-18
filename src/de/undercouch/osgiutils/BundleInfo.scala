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
import de.undercouch.scalahelpers.StringAlgorithm._

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
  
  case class ImportDeclaration(name: String, optional: Boolean, version: VersionRange, bundleSymbolicName: Option[String], bundleVersion: VersionRange)
  
  object ImportDeclaration {
    def apply(name: String, optional: Boolean, version: VersionRange): ImportDeclaration =
      ImportDeclaration(name, optional, version, None, VersionRange.Default)
  }
  
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
  
  private def parseManifestEntry(v: String): Header =
    v.splitIf(((_: Char) == ',') withQuotes) map (v => (v.split(";")) map (_.trim))
  
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
  
  private def parseImportedPackages(manifest: Manifest): Array[ImportDeclaration] = {
    var allNames = Set[String]()
    
    sealed trait ParsedDecl
    case class ParsedPackage(name: String) extends ParsedDecl
    case class ParsedDirective(name: String, value: String) extends ParsedDecl
    case class ParsedParam(name: String, value: String) extends ParsedDecl
    
    object DeclParser extends RegexParsers {
      lazy val decl = directive | param | pkg
      lazy val directive = dname ~ ":=" ~ pvalue ^^ { case n ~ ":=" ~ v => ParsedDirective(n, v) }
      lazy val param = pname ~ "=" ~ pvalue ^^ { case n ~ "=" ~ v => ParsedParam(n, v) }
      lazy val dname = "resolution" | regex("[^\\:]+"r)
      lazy val pname = "version" | "specification-version" | "bundle-symbolic-name" | "bundle-version" | regex("[^=]+"r)
      lazy val pvalue = "\"" ~> regex("[^\"]*"r) <~ "\"" | regex(".*"r)
      lazy val pkg = regex(".*"r) ^^ { ParsedPackage(_) }
    }
    
    def parsePackageDeclaration(decl: HeaderClause): Set[ImportDeclaration] = {
      var names = Set[String]()
      var optional = false
      var version = VersionRange.Default
      var bundleSymbolicName: Option[String] = None
      var bundleVersion = VersionRange.Default
      for (d <- decl) DeclParser.decl(new CharSequenceReader(d)) match {
        case DeclParser.Success(result, next) if next.atEnd => result match {
          case ParsedPackage(name) =>
            if (allNames contains name) {
              throw new InvalidBundleException("Duplicate import package: " + name)
            } else {
              allNames += name
              names += name
            }
            
          case ParsedDirective(name, value) => name.trim match {
            case "resolution" => optional = parseResolution(value.trim)
            case _ => //ignore
          }
          
          case ParsedParam(name, value) => name.trim match {
            case "version" => version = VersionRange(value.trim)
            case "specification-version" =>
              val sv = VersionRange(value.trim)
              if (version == VersionRange.Default)
                version = sv
              else if (version != sv)
                throw new InvalidBundleException("Import package specification version must match version: " + sv + " != " + version)
            case "bundle-symbolic-name" =>
              bundleSymbolicName = Some(value.trim)
            case "bundle-version" =>
              bundleVersion = VersionRange(value.trim)
            case _ => //ignore
          }
        }
        case s => throw new InvalidBundleException("Invalid import package declaration: " + d)
      }
        
      for (n <- names) yield ImportDeclaration(n, optional, version, bundleSymbolicName, bundleVersion)
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
