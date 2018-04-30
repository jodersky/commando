// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.{crossProject, CrossType}

lazy val commando = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "utest" % "0.6.3" % "test"
    ),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    scalaVersion := crossScalaVersions.value.head
  )
  .jsSettings(
    crossScalaVersions := "2.12.5" :: "2.11.12" :: Nil
  )
  .jvmSettings(
    crossScalaVersions := "2.12.5" :: "2.11.12" :: Nil
  )
  .nativeSettings(
    crossScalaVersions := "2.11.12" :: Nil,
    nativeLinkStubs := true
  )

lazy val commandoJS = commando.js
lazy val commandoJVM = commando.jvm
lazy val commandoNative = commando.native
