import sbt._

class OSGiUtilitiesProject(info: ProjectInfo) extends DefaultProject(info)
{
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC3-SNAPSHOT" % "test"
  val junit = "junit" % "junit" % "4.5" % "test"

  override def mainScalaSourcePath = "src"
  override def mainResourcesPath = "src"
   
  override def testScalaSourcePath = "test"
  override def testResourcesPath = "test"
}
