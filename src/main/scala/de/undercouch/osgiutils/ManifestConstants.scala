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
 * Well-known identifiers used in bundle manifests
 * @author Michel Kraemer
 */
object ManifestConstants {
  //R4 headers
  val BundleActivationPolicy = "Bundle-ActivationPolicy"
  val BundleActivator = "Bundle-Activator"
  val BundleCategory = "Bundle-Category"
  val BundleClassPath = "Bundle-ClassPath"
  val BundleContactAddress = "Bundle-ContactAddress"
  val BundleCopyright = "Bundle-Copyright"
  val BundleDescription = "Bundle-Description"
  val BundleDocURL = "Bundle-DocURL"
  val BundleIcon = "Bundle-Icon"
  val BundleLicense = "Bundle-License"
  val BundleLocalization = "Bundle-Localization"
  val BundleManifestVersion = "Bundle-ManifestVersion"
  val BundleName = "Bundle-Name"
  val BundleNativeCode = "Bundle-NativeCode"
  val BundleRequiredExecutionEnvironment = "Bundle-RequiredExecutionEnvironment"
  val BundleSymbolicName = "Bundle-SymbolicName"
  val BundleUpdateLocation = "Bundle-UpdateLocation"
  val BundleVendor = "Bundle-Vendor"
  val BundleVersion = "Bundle-Version"
  val DynamicImportPackage = "DynamicImport-Package"
  val ExportPackage = "Export-Package"
  val ExportService = "Export-Service"
  val FragmentHost = "Fragment-Host"
  val ImportPackage = "Import-Package"
  val ImportService = "Import-Service"
  val RequireBundle = "Require-Bundle"
}