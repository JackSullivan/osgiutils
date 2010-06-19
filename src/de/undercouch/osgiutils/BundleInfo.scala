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
  /**
   * Retrieves a parsed entry from the bundle's manifest
   * @param name the entry's name
   * @return the parsed entry (may be empty if an entry with
   * the given name does not exist in the manifest)
   */
  def getManifestEntry(name: String): BundleInfo.Header =
    BundleInfo.parseManifestEntry(manifest, name)
  
  /**
   * Retrieves the value of an entry from the bundle's manifest
   * @param name the entry's name
   * @return the entry's value of None of an entry with
   * the given name does not exist in the manifest
   */
  def getSimpleManifestEntry(name: String): Option[String] =
    BundleInfo.getSimpleManifestEntry(manifest, name)
}

/**
 * Provides methods to parse bundle manifests
 * @author Michel Kraemer
 */
object BundleInfo {
  /**
   * Aliases for bundle manifest header items
   */
  type HeaderClause = Array[String]
  type Header = Array[HeaderClause]
  
  /**
   * A version declaration (in the format major.minor.micro.qualifier)
   * @author Michel Kraemer
   */
  case class Version(major: Int, minor: Int, micro: Int, qualifier: Option[String])
  
  /**
   * Defines methods to parse version declarations
   * @author Michel Kraemer
   */
  object Version {
    /**
     * The default version (0.0.0)
     */
    val Default = Version(0, 0, 0, None)
    
    /**
     * A version number that is infinitely large
     */
    val Infinite = new Version(0, 0, 0, None) {
      //TODO overwrite comparison operators
    }
    
    /**
     * Creates a new version number
     * @param major the major version number
     * @return the version number
     */
    def apply(major: Int): Version =
      Version(major, 0, 0, None)
    
    /**
     * Creates a new version number
     * @param major the major version number
     * @param minor the minor version number
     * @return the version number
     */
    def apply(major: Int, minor: Int): Version =
      Version(major, minor, 0, None)
      
    /**
     * Creates a new version number
     * @param major the major version number
     * @param minor the minor version number
     * @param micro the micro version number
     * @return the version number
     */
    def apply(major: Int, minor: Int, micro: Int): Version =
      Version(major, minor, micro, None)
    
    /**
     * Creates a new version number
     * @param major the major version number
     * @param minor the minor version number
     * @param micro the micro version number
     * @param qualifier the version qualifier
     * @return the version number
     */
    def apply(major: Int, minor: Int, micro: Int, qualifier: String): Version =
      Version(major, minor, micro, if (qualifier != null) Some(qualifier) else None)
      
    /**
     * Parses a version number string
     * @param v the string to parse
     * @return the version number
     * @throws InvalidBundleException if the version number string is invalid
     */
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
  
  /**
   * A version range
   * @author Michel Kraemer
   */
  case class VersionRange(floor: Version, ceiling: Version, floorInclusive: Boolean, ceilingInclusive: Boolean)
  
  /**
   * Defines methods to parse version ranges
   * @author Michel Kraemer
   */
  object VersionRange {
    /**
     * A default version range from Version.Default (inclusively) to Version.Infinite
     */
    val Default = VersionRange(Version.Default)
    
    /**
     * Defines the new version range which that starts at the
     * given version number (inclusively) and goes to Version.Infinite
     * @param v the lower version number
     * @return the new version range
     */
    def apply(v: Version): VersionRange =
      VersionRange(v, Version.Infinite, true, false)
      
    /**
     * Parses a version range string
     * @param v the string to parse
     * @return the parsed version range
     * @throws InvalidBundleException if the version range string is invalid
     */
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
  }
  
  /**
   * A package import declaration
   * @author Michel Kraemer
   */
  case class ImportDeclaration(name: String, optional: Boolean, version: VersionRange,
    bundleSymbolicName: Option[String], bundleVersion: VersionRange)
  
  /**
   * Defines methods to create package import declarations
   * @author Michel Kraemer
   */
  object ImportDeclaration {
    /**
     * Creates a new package import declaration with the given parameters and
     * no bundle symbolic name and no bundle version
     * @param name the name of the package to import
     * @param optional true if the import is optional
     * @param version the version of the imported package
     * @return the new import declaration
     */
    def apply(name: String, optional: Boolean, version: VersionRange): ImportDeclaration =
      ImportDeclaration(name, optional, version, None, VersionRange.Default)
  }
  
  /**
   * Fills a BundleInfo object with values from the
   * given Manifest
   * @param manifest the Manifest to parse
   */
  def apply(manifest: Manifest): BundleInfo = BundleInfo(
      manifest,
      BundleInfo.parseManifestVersion(manifest),
      BundleInfo.parseSymbolicName(manifest),
      BundleInfo.getSimpleManifestEntry(manifest, ManifestConstants.BundleName),
      BundleInfo.getSimpleManifestEntry(manifest, ManifestConstants.BundleDescription),
      BundleInfo.parseVersion(manifest),
      BundleInfo.parseImportedPackages(manifest)
  )
  
  /**
   * Parses a manifest entry to an array of strings
   * @param manifest the manifest
   * @param name the name of the entry to parse
   * @return the parsed entry or an empty array if the entry does
   * not exist in the given manifest
   */
  private def parseManifestEntry(manifest: Manifest, name: String): Header = {
    val v = getSimpleManifestEntry(manifest, name)
    if (v.isDefined) parseManifestEntry(v.get) else Array.empty
  }
  
  /**
   * Retrieves the value of a manifest entry
   * @param manifest the manifest
   * @param name the name of the entry
   * @return the entry's value or None if the entry does
   * not exist in the given manifest
   */
  private def getSimpleManifestEntry(manifest: Manifest, name: String): Option[String] = {
    val attrs = manifest.getMainAttributes()
    if (attrs == null) None else getSimpleManifestEntry(attrs, name)
  }
  
  /**
   * Retrieves the value of a manifest entry from an attribute map
   * @param attrs the attribute map
   * @param name the name of the entry
   * @return the entry's value or None if the entry does
   * not exist in the given attribute map
   */
  private def getSimpleManifestEntry(attrs: Attributes, name: String): Option[String] = {
    val v = attrs.getValue(name)
    if (v == null) None else Some(v)
  }
  
  /**
   * Parses a single manifest entry value
   * @param v the value to parse
   * @return the parsed header clauses
   */
  private def parseManifestEntry(v: String): Header =
    v.splitIf(((_: Char) == ',') withQuotes) map (v => (v.split(";")) map (_.trim))
  
  /**
   * Retrieves the manifest version
   * @param manifest the manifest
   * @return the manifest version or 1 if there is no version defined
   * @throws InvalidBundleException if the version declaration is invalid
   */
  private def parseManifestVersion(manifest: Manifest): Int = {
    val oh = getSimpleManifestEntry(manifest, ManifestConstants.BundleManifestVersion)
    try {
      oh map (_.toInt) getOrElse 1
    } catch {
      case _: NumberFormatException =>
        throw new InvalidBundleException("Invalid manifest version: " + oh.get)
    }
  }
  
  /**
   * Parses the symbolic name of a bundle
   * @param manifest the bundle manifest
   * @return the symbolic name
   * @throws InvalidBundleException if the bundle manifest
   * does not contain a symbolic name
   */
  private def parseSymbolicName(manifest: Manifest): String = {
    val h = parseManifestEntry(manifest, ManifestConstants.BundleSymbolicName)
    if (h.isEmpty || h(0).isEmpty) {
      throw new InvalidBundleException("Manifest contains no symbolic name")
    }
    h(0)(0)
  }
  
  /**
   * Parses the version of a bundle
   * @param manifest the bundle manifest
   * @return the parsed version or Version.Default if the
   * bundle manifest does not contain a version number
   */
  private def parseVersion(manifest: Manifest): Version = {
    val oh = getSimpleManifestEntry(manifest, ManifestConstants.BundleVersion)
    oh map Version.apply getOrElse Version.Default
  }
  
  /**
   * Parses the imported packages of a bundle
   * @param manifest the bundle manifest
   * @return the parsed package import declarations
   * @throws InvalidBundleException if any of the import
   * declarations is invalid
   */
  private def parseImportedPackages(manifest: Manifest): Array[ImportDeclaration] = {
    //a set of already imported packages
    var allNames = Set[String]()
    
    //parser result classes
    sealed trait ParsedDecl
    case class ParsedPackage(name: String) extends ParsedDecl
    case class ParsedDirective(name: String, value: String) extends ParsedDecl
    case class ParsedParam(name: String, value: String) extends ParsedDecl
    
    //parses import declarations
    object DeclParser extends RegexParsers {
      lazy val decl = directive | param | pkg
      lazy val directive = dname ~ ":=" ~ pvalue ^^ { case n ~ ":=" ~ v => ParsedDirective(n, v) }
      lazy val param = pname ~ "=" ~ pvalue ^^ { case n ~ "=" ~ v => ParsedParam(n, v) }
      lazy val dname = "resolution" | regex("[^\\:]+"r)
      lazy val pname = "version" | "specification-version" | "bundle-symbolic-name" | "bundle-version" | regex("[^=]+"r)
      lazy val pvalue = "\"" ~> regex("[^\"]*"r) <~ "\"" | regex(".*"r)
      lazy val pkg = regex(".*"r) ^^ { ParsedPackage(_) }
    }
    
    /**
     * Parses an import package declaration
     * @param decl the header to parse
     * @return the parsed declarations
     * @throws InvalidBundleException if the import header is invalid
     */
    def parsePackageDeclaration(decl: HeaderClause): Set[ImportDeclaration] = {
      //parse result variables
      var names = Set[String]()
      var optional = false
      var version = VersionRange.Default
      var bundleSymbolicName: Option[String] = None
      var bundleVersion = VersionRange.Default
      
      //parse each header clause
      for (d <- decl) DeclParser.decl(new CharSequenceReader(d)) match {
        //parser success
        case DeclParser.Success(result, next) if next.atEnd => result match {
          case ParsedPackage(name) =>
            if (allNames contains name) {
              //package has already been imported
              throw new InvalidBundleException("Duplicate import package: " + name)
            } else {
              //save imported package
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
        
        //parser error
        case _ => throw new InvalidBundleException("Invalid import package declaration: " + d)
      }
      
      //wrap result variables into import declarations
      for (n <- names) yield ImportDeclaration(n, optional, version, bundleSymbolicName, bundleVersion)
    }
    
    //get import package headers and parse them
    val h = parseManifestEntry(manifest, ManifestConstants.ImportPackage)
    h flatMap parsePackageDeclaration
  }
  
  /**
   * Parse the resolution directive of an import package declaration
   * @param v the value of the resolution directive
   * @return true of false depending on if the package import is
   * optional or mandatory respectively
   * @throws InvalidBundleException if the given value is unknown
   */
  private def parseResolution(v: String) = v match {
    case "optional" => true
    case "mandatory" => false
    case _ => throw new InvalidBundleException("Invalid value for resolution attribute" + v)
  }
}
