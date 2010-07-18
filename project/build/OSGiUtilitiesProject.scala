import sbt._
import java.io.FileInputStream
import java.util.jar.Manifest

class OSGiUtilitiesProject(info: ProjectInfo) extends DefaultProject(info) {
  val undercouchRepo = "underCOUCH Maven Repository" at "http://www.undercouch.de/repo"
  val scalahelpers = "de.undercouch" % "scalahelpers" % "1.0.1"
  
  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
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
  
  //include OSGi Manifest
  val manifest = new Manifest(new FileInputStream("META-INF/MANIFEST.MF"))
  override def packageOptions = super.packageOptions ++ Seq(JarManifest(manifest))
}
