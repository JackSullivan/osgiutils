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
 * A package export declaration
 * @author Michel Kraemer
 */
case class ExportDeclaration(name: String, version: Version = Version.Default,
  uses: Set[String] = Set.empty, mandatoryAttributes: Set[String] = Set.empty,
  includedClasses: Set[String] = Set.empty, excludedClasses: Set[String] = Set.empty,
  matchingAttributes: Map[String, String] = Map.empty)
