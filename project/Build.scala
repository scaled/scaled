import sbt._
import Keys._

object ScaledBuild extends samskivert.MavenBuild {

  override val globalSettings = seq(
    crossPaths       := false,
    fork in Compile  := true,
    autoScalaLibrary := false, // scala-library depend comes from POM

    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-optimize",
                          "-language:postfixOps" /*, "-Yinline-warnings"*/),

    // TEMP: until we ship a new sbt-pom-util, we have to repeat scala version here
    scalaVersion := "2.11.0",

    // wires JUnit into SBT
    libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test->default",
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v")
  ) ++ net.virtualvoid.sbt.graph.Plugin.graphSettings

  override def moduleSettings (name :String, pom :pomutil.POM) = name match {
    case "devel" => spray.revolver.RevolverPlugin.Revolver.settings
    case _       => Nil
  }

  override def profiles = Seq("devel")
  // override def profiles = Seq("devel", "bootstrap")
}
