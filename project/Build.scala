//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

import sbt._
import Keys._

object ScaledBuild extends Build {

  val localSettings = seq(
    crossPaths    := false,
    scalacOptions ++= Seq("-unchecked", "-deprecation"),
    autoScalaLibrary := false, // we manually add scala-library in our POMs
    fork in Compile := true,
    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.10" % "test" // make junit work
    ),
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v")
  )

  lazy val root = Project("scaled", file("."), settings = {
    Project.defaultSettings ++
    samskivert.POMUtil.pomToSettings("pom.xml") ++
    net.virtualvoid.sbt.graph.Plugin.graphSettings ++
    spray.revolver.RevolverPlugin.Revolver.settings ++
    localSettings
  })
}
