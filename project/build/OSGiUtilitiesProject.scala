import sbt._

class OSGiUtilitiesProject(info: ProjectInfo) extends DefaultProject(info)
{
  val undercouchRepo = "underCOUCH Maven Repository" at "http://www.undercouch.de/repo"
  val scalahelpers = "de.undercouch" % "scalahelpers" % "1.0"
  
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC3-SNAPSHOT" % "test"
  val junit = "junit" % "junit" % "4.5" % "test"

  override def mainScalaSourcePath = "src"
  override def mainResourcesPath = "src"
   
  override def testScalaSourcePath = "test"
  override def testResourcesPath = "test"
}
