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
 * Represents a bundle required by another bundle
 * @author Michel Kraemer
 */
case class RequiredBundle(@BeanProperty symbolicName: String,
  @BooleanBeanProperty optional: Boolean = false,
  @BeanProperty version: VersionRange = VersionRange.Default,
  @BooleanBeanProperty reexport: Boolean = false)
