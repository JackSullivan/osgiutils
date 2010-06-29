import sbt._
import com.weiglewilczek.bnd4sbt.BNDPlugin

class OSGiUtilitiesProject(info: ProjectInfo) extends DefaultProject(info) with BNDPlugin
{
  val undercouchRepo = "underCOUCH Maven Repository" at "http://www.undercouch.de/repo"
  val scalahelpers = "de.undercouch" % "scalahelpers" % "1.0"
  
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC6-SNAPSHOT" % "test"
  val junit = "junit" % "junit" % "4.5" % "test"

  override def mainScalaSourcePath = "src"
  override def mainResourcesPath = "src"
   
  override def testScalaSourcePath = "test"
  override def testResourcesPath = "test"
  
  //configure artifact
  override def moduleID = "osgiutils"
  
  //publish sources
  override def packageSrcJar = defaultJarPath("-sources.jar")
  val sourceArtifact = Artifact.sources(artifactID)
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageSrc)
  
  //configure OSGi bundle
  override def bndBundleName = "OSGi Utilities"
  override def bndBundleVendor = Some("Michel Kraemer")
  override def bndExportPackage = Set("de.undercouch.osgiutils;version=" + projectVersion.value,
    "de.undercouch.osgiutils.registry;version=" + projectVersion.value)
  override def bndImportPackage = Set()
  override def bndPrivatePackage = Set()
  override def bndIncludeResource = Set()
}
