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

/**
 * A package import declaration
 * @author Michel Kraemer
 */
case class ImportDeclaration(name: String, optional: Boolean, version: VersionRange,
  bundleSymbolicName: Option[String], bundleVersion: VersionRange, matchingAttributes: Map[String, String])

/**
 * Defines methods to create package import declarations
 * @author Michel Kraemer
 */
object ImportDeclaration {
  /**
   * Creates a new package import declaration with the given parameters and
   * no bundle symbolic name and no bundle version and no other matching attributes
   * @param name the name of the package to import
   * @param optional true if the import is optional
   * @param version the version of the imported package
   * @return the new import declaration
   */
  def apply(name: String, optional: Boolean, version: VersionRange): ImportDeclaration =
    ImportDeclaration(name, optional, version, None, VersionRange.Default, Map.empty)
}
