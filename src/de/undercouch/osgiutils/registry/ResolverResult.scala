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

import scala.reflect.{BeanProperty, BooleanBeanProperty}
import de.undercouch.osgiutils.BundleInfo

/**
 * The result of the bundle resolving process
 * @author Michel Kraemer
 */
case class ResolverResult(@BeanProperty bundle: BundleInfo,
  @BooleanBeanProperty resolved: Boolean = false)
