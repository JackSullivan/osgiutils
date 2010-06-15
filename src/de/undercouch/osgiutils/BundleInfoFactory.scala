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

import java.io.{File, InputStream}
import java.net.{JarURLConnection, URL}
import java.util.jar.{JarFile, Manifest}

/**
 * A factory for {@link BundleInfo} objects. Reads
 * information from bundles stored in directories
 * or .jar files.
 * @author Michel Kraemer
 */
object BundleInfoFactory {
  /**
   * Creates a {@link BundleInfo} object either from a .jar file
   * containing a bundle or a MANIFEST.MF file.
   * @param url the URL to the .jar file or to the MANIFEST.MF file
   * @return the {@link BundleInfo} object
   */
  def createBundleInfo(url: URL): BundleInfo = url.openConnection() match {
    case juc: JarURLConnection =>
      createBundleInfoFromJar(juc.getJarFile())
    case m =>
      createBundleInfoFromManifest(m.getInputStream())
  }
  
  /**
   * Creates a {@link BundleInfo} object from a MANIFEST.MF file.
   * @param url the URL to the MANIFEST.MF file
   * @return the {@link BundleInfo} object
   */
  def createBundleInfoFromManifest(url: URL): BundleInfo =
    createBundleInfoFromManifest(url.openConnection().getInputStream())
  
  /**
   * Creates a {@link BundleInfo} object from a MANIFEST.MF file.
   * @param is the {@link InputStream} used to read the MANIFEST.MF file
   * @return the {@link BundleInfo} object
   */
  def createBundleInfoFromManifest(is: InputStream): BundleInfo =
    createBundleInfoFromManifest(new Manifest(is))
    
  /**
   * Creates a {@link BundleInfo} object from a manifest
   * @param manifest the manifest
   * @return the {@link BundleInfo} object
   */
  def createBundleInfoFromManifest(manifest: Manifest): BundleInfo =
    new BundleInfo(manifest)
  
  /**
   * Creates a {@link BundleInfo} object from a .jar file
   * containing a bundle
   * @param url the URL to the .jar file
   * @return the {@link BundleInfo} object
   */
  def createBundleInfoFromJar(jar: JarFile): BundleInfo =
    createBundleInfoFromManifest(jar.getManifest())
  
  /**
   * Creates a {@link BundleInfo} object from a directory
   * containing a bundle
   * @param path the path to the bundle directory
   * @return the {@link BundleInfo} object
   */
  def createBundleInfoFromDirectory(path: File): BundleInfo = {
    //new BundleInfo("")
    //TODO
    null
  }
}