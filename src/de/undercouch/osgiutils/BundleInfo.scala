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

import java.io.{File, FileInputStream, InputStream}
import java.net.URL
import java.util.jar.{Attributes, JarFile, JarInputStream, Manifest}
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
  val version: Version,
  
  /**
   * The bundle's fragment host
   */
  @BeanProperty
  val fragmentHost: Option[FragmentHost],
  
  /**
   * The packages exported by this bundle
   */
  @BeanProperty
  val exportedPackages: List[ExportedPackage],

  /**
   * The packages imported by this bundle
   */
  @BeanProperty
  val importedPackages: List[ImportedPackage],
  
  /**
   * The bundles required by this bundle
   */
  @BeanProperty
  val requiredBundles: List[RequiredBundle]
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
  type HeaderClause = List[String]
  type Header = List[HeaderClause]
  
  //header parser result classes
  private sealed trait ParsedHeaderDecl
  private case class ParsedHeader(name: String) extends ParsedHeaderDecl
  private case class ParsedDirective(name: String, value: String) extends ParsedHeaderDecl
  private case class ParsedParam(name: String, value: String) extends ParsedHeaderDecl
  
  //parses header declarations
  private object HeaderDeclParser extends RegexParsers {
    lazy val decl = directive | param | header
    lazy val directive = dname ~ ":=" ~ pvalue ^^ { case n ~ ":=" ~ v => ParsedDirective(n, v) }
    lazy val param = pname ~ "=" ~ pvalue ^^ { case n ~ "=" ~ v => ParsedParam(n, v) }
    lazy val dname = regex("[^\\:]+"r)
    lazy val pname = regex("[^=]+"r)
    lazy val pvalue = "\"" ~> regex("[^\"]*"r) <~ "\"" | regex(".*"r)
    lazy val header = regex(".*"r) ^^ { ParsedHeader(_) }
  }
  
  /**
   * Creates a {@link BundleInfo} object from a MANIFEST.MF file.
   * @param url the URL to the MANIFEST.MF file
   * @return the {@link BundleInfo} object
   */
  def fromManifest(url: URL): BundleInfo =
    fromManifest(url.openConnection().getInputStream())
  
  /**
   * Creates a {@link BundleInfo} object from a MANIFEST.MF file.
   * @param is the {@link InputStream} used to read the MANIFEST.MF file
   * @return the {@link BundleInfo} object
   */
  def fromManifest(is: InputStream): BundleInfo =
    fromManifest(new Manifest(is))
  
  /**
   * Creates a {@link BundleInfo} object from a manifest
   * @param manifest the manifest
   * @return the {@link BundleInfo} object
   */
  def fromManifest(manifest: Manifest): BundleInfo = BundleInfo(
      manifest,
      BundleInfo.parseManifestVersion(manifest),
      BundleInfo.parseSymbolicName(manifest),
      BundleInfo.getSimpleManifestEntry(manifest, ManifestConstants.BundleName),
      BundleInfo.getSimpleManifestEntry(manifest, ManifestConstants.BundleDescription),
      BundleInfo.parseVersion(manifest),
      BundleInfo.parseFragmentHost(manifest),
      BundleInfo.parseExportedPackages(manifest),
      BundleInfo.parseImportedPackages(manifest),
      BundleInfo.parseRequiredBundles(manifest)
  )
  
  /**
   * Creates a {@link BundleInfo} object from a bundle .jar file
   * @param url the URL to the .jar file
   * @return the {@link BundleInfo} object
   */
  def fromJar(url: URL): BundleInfo = {
    val is = url.openConnection().getInputStream()
    val jis = new JarInputStream(is)
    fromManifest(jis.getManifest)
  }
  
  /**
   * Creates a {@link BundleInfo} object from a .jar file
   * containing a bundle
   * @param url the URL to the .jar file
   * @return the {@link BundleInfo} object
   */
  def fromJar(jar: JarFile): BundleInfo =
    fromManifest(jar.getManifest())
  
  /**
   * Creates a {@link BundleInfo} object from a directory
   * containing a bundle
   * @param path the path to the bundle directory
   * @return the {@link BundleInfo} object
   */
  def fromDirectory(path: File): BundleInfo = {
    val manifestFile = new File(path, "META-INF/MANIFEST.MF")
    val is = new FileInputStream(manifestFile)
    fromManifest(is)
  }
  
  /**
   * Parses a manifest entry to an array of strings
   * @param manifest the manifest
   * @param name the name of the entry to parse
   * @return the parsed entry or an empty array if the entry does
   * not exist in the given manifest
   */
  private def parseManifestEntry(manifest: Manifest, name: String): Header = {
    val v = getSimpleManifestEntry(manifest, name)
    if (v.isDefined) parseManifestEntry(v.get) else List.empty
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
    (v.splitIf(((_: Char) == ',') withQuotes).toList) map (v => (v.split(";").toList) map (_.trim))
  
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
   * Parses the fragment host of a bundle
   * @param manifest the bundle manifest
   * @return the fragment host or None if the bundle does not have one
   */
  private def parseFragmentHost(manifest: Manifest): Option[FragmentHost] = {
    val fh = parseManifestEntry(manifest, ManifestConstants.FragmentHost)
    if (fh.isEmpty) {
      None
    } else {
      //handle one host only
      val host = fh(0)
      if (host.isEmpty) {
        None
      } else {
        var name = ""
        var version = VersionRange.Default
        var extension = FragmentHost.Extension.None
        for (d <- host) HeaderDeclParser.decl(new CharSequenceReader(d)) match {
          //parser success
          case HeaderDeclParser.Success(result, next) if next.atEnd => result match {
            case ParsedHeader(n) =>
              if (name.isEmpty)
                name = n
              else
                throw new InvalidBundleException("More than one fragment host defined: " + name + ", " + n)
              
            case ParsedDirective(name, value) => name.trim match {
              case "extension" => extension = parseExtension(value.trim)
              case _ => //ignore
            }
            
            case ParsedParam(name, value) => name.trim match {
              case "bundle-version" => version = VersionRange(value.trim)
              case _ => //ignore
            }
          }
          
          //parser error
          case _ => throw new InvalidBundleException("Invalid fragment host declaration: " + d)
        }
        Some(FragmentHost(name, version, extension))
      }
    }
  }
  
  /**
   * Parses the imported packages of a bundle
   * @param manifest the bundle manifest
   * @return the parsed package import declarations
   * @throws InvalidBundleException if any of the import
   * declarations is invalid
   */
  private def parseImportedPackages(manifest: Manifest): List[ImportedPackage] = {
    //a set of already imported packages
    var allNames = Set[String]()
    
    /**
     * Parses an import package declaration
     * @param decl the header to parse
     * @return the parsed declarations
     * @throws InvalidBundleException if the import header is invalid
     */
    def parsePackageDeclaration(decl: HeaderClause): Set[ImportedPackage] = {
      //parse result variables
      var names = Set[String]()
      var optional = false
      var version = VersionRange.Default
      var bundleSymbolicName: Option[String] = None
      var bundleVersion = VersionRange.Default
      var matchingAttributes = Map[String, String]()
      
      //parse each header clause
      for (d <- decl) HeaderDeclParser.decl(new CharSequenceReader(d)) match {
        //parser success
        case HeaderDeclParser.Success(result, next) if next.atEnd => result match {
          case ParsedHeader(name) =>
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
            case _ => matchingAttributes += (name -> value)
          }
        }
        
        //parser error
        case _ => throw new InvalidBundleException("Invalid import package declaration: " + d)
      }
      
      //wrap result variables into import declarations
      for (n <- names) yield ImportedPackage(n, optional, version, bundleSymbolicName,
        bundleVersion, matchingAttributes)
    }
    
    //get import package headers and parse them
    val h = parseManifestEntry(manifest, ManifestConstants.ImportPackage)
    h flatMap parsePackageDeclaration
  }
  
  /**
   * Parses the exported packages of a bundle
   * @param manifest the bundle manifest
   * @return the parsed package export declarations
   * @throws InvalidBundleException if any of the export
   * declarations is invalid
   */
  private def parseExportedPackages(manifest: Manifest): List[ExportedPackage] = {
    /**
     * Parses an export package declaration
     * @param decl the header to parse
     * @return the parsed declarations
     * @throws InvalidBundleException if the export header is invalid
     */
    def parsePackageDeclaration(decl: HeaderClause): List[ExportedPackage] = {
      //parse result variables
      var names = List[String]()
      var version = Version.Default
      var uses = Set[String]()
      var mandatory = Set[String]()
      var includedClasses = Set[String]()
      var excludedClasses = Set[String]()
      var matchingAttributes = Map[String, String]()
      
      //parse each header clause
      for (d <- decl) HeaderDeclParser.decl(new CharSequenceReader(d)) match {
        //parser success
        case HeaderDeclParser.Success(result, next) if next.atEnd => result match {
          case ParsedHeader(name) =>
            names ::= name
            
          case ParsedDirective(name, value) =>
            val s = (value.split(",") map (_.trim)).toSet
            name.trim match {
              case "uses" => uses = s
              case "mandatory" => mandatory = s
              case "include" => includedClasses = s
              case "exclude" => excludedClasses = s
              case _ => //ignore
            }
          
          case ParsedParam(name, value) => name.trim match {
            case "version" => version = Version(value.trim)
            case "specification-version" =>
              val sv = Version(value.trim)
              if (version == Version.Default)
                version = sv
              else if (version != sv)
                throw new InvalidBundleException("Export package specification version must match version: " + sv + " != " + version)
            case _ => matchingAttributes += (name -> value)
          }
        }
        
        //parser error
        case _ => throw new InvalidBundleException("Invalid export package declaration: " + d)
      }
      
      //wrap result variables into export declarations
      for (n <- names) yield ExportedPackage(n, version, uses, mandatory,
        includedClasses, excludedClasses, matchingAttributes)
    }
    
    val h = parseManifestEntry(manifest, ManifestConstants.ExportPackage)
    h flatMap parsePackageDeclaration
  }
  
  /**
   * Parses the required bundles of a bundle
   * @param manifest the bundle manifest
   * @return the required bundles
   */
  private def parseRequiredBundles(manifest: Manifest): List[RequiredBundle] = {
    /**
     * Parses a require-bundle declaration
     * @param decl the header to parse
     * @return the parsed declarations
     * @throws InvalidBundleException if the require-bundle header is invalid
     */
    def parseBundleDeclaration(decl: HeaderClause): RequiredBundle = {
      var name = ""
      var optional = false
      var version = VersionRange.Default
      var reexport = false
      
      //parse each header clause
      for (d <- decl) HeaderDeclParser.decl(new CharSequenceReader(d)) match {
        //parser success
        case HeaderDeclParser.Success(result, next) if next.atEnd => result match {
          case ParsedHeader(n) =>
            if (name.isEmpty)
              name = n
            else
              throw new InvalidBundleException("More than one required bundle defined " +
              		"in one header clause: " + name + ", " + n)
          
          case ParsedDirective(name, value) => name.trim match {
            case "visibility" => reexport = parseVisibility(value.trim)
            case "resolution" => optional = parseResolution(value.trim)
            case _ => //ignore
          }
          
          case ParsedParam(name, value) => name.trim match {
            case "bundle-version" => version = VersionRange(value.trim)
            case _ => //ignore
          }
        }
        
        //parser error
        case _ => throw new InvalidBundleException("Invalid require-bundle declaration: " + d)
      }
      
      RequiredBundle(name, optional, version, reexport)
    }
    
    val h = parseManifestEntry(manifest, ManifestConstants.RequireBundle)
    h map parseBundleDeclaration
  }
  
  /**
   * Parse the resolution directive of an import package declaration
   * or require bundle declaration respectively
   * @param v the value of the resolution directive
   * @return true of false depending on if the package import is
   * optional or mandatory respectively
   * @throws InvalidBundleException if the given value is unknown
   */
  private def parseResolution(v: String) = v match {
    case "optional" => true
    case "mandatory" => false
    case _ => throw new InvalidBundleException("Invalid value for resolution attribute: " + v)
  }
  
  /**
   * Parse the extension directive of a fragment host declaration
   * @param v the value of the extension directive
   * @return the parsed extension value
   * @throws InvalidBundleException if the given value is unknown
   */
  private def parseExtension(v: String) = v match {
    case "framework" => FragmentHost.Extension.Framework
    case "bootclasspath" => FragmentHost.Extension.BootClassPath
    case _ => throw new InvalidBundleException("Invalid value for extension attribute: " + v)
  }
  
  /**
   * Parse the visibility directive of an require bundle declaration
   * @param v the value of the visibility directive
   * @return true of false depending on if the packages of
   * the required bundle should be re-exported or not
   * @throws InvalidBundleException if the given value is unknown
   */
  private def parseVisibility(v: String) = v match {
    case "reexport" => true
    case "private" => false
    case _ => throw new InvalidBundleException("Invalid value for visibility attribute: " + v)
  }
}
