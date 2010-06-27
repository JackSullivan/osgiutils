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
 * This exception will be thrown by the BundleRegistry during the resolving process
 * @author Michel Kraemer
 */
abstract class ResolverException(msg: String) extends RuntimeException(msg, null)

/**
 * This exception will be thrown by the BundleRegistry when a dependency cycle
 * has been detected. The path contains the bundles that make up the cycle whereas
 * the first entry in this array equals the last one to close the cycle.
 */
class DependencyCycleException(msg: String,
  @BeanProperty val path: Array[BundleInfo]) extends ResolverException(msg)
