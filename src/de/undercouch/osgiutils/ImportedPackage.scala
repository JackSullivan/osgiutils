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

import scala.reflect.{BeanProperty, BooleanBeanProperty}

/**
 * A package import declaration
 * @author Michel Kraemer
 */
case class ImportedPackage(@BeanProperty name: String,
  @BooleanBeanProperty optional: Boolean = false,
  @BeanProperty version: VersionRange = VersionRange.Default,
  @BeanProperty bundleSymbolicName: Option[String] = None,
  @BeanProperty bundleVersion: VersionRange = VersionRange.Default,
  @BeanProperty matchingAttributes: Map[String, String] = Map.empty)
