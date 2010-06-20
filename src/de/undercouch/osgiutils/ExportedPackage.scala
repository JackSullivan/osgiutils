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

import scala.reflect.BeanProperty

/**
 * A package export declaration
 * @author Michel Kraemer
 */
case class ExportedPackage(@BeanProperty name: String,
  @BeanProperty version: Version = Version.Default,
  @BeanProperty uses: Set[String] = Set.empty,
  @BeanProperty mandatoryAttributes: Set[String] = Set.empty,
  @BeanProperty includedClasses: Set[String] = Set.empty,
  @BeanProperty excludedClasses: Set[String] = Set.empty,
  @BeanProperty matchingAttributes: Map[String, String] = Map.empty)
