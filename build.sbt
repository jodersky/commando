scalaVersion := "2.12.5"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "utest" % "0.6.3" % "test"
)
testFrameworks += new TestFramework("utest.runner.Framework")
