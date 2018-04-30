organization in ThisBuild := "io.crashbox"
licenses in ThisBuild := Seq(
  ("BSD-3-Clause", url("https://opensource.org/licenses/BSD-3-Clause")))
homepage in ThisBuild := Some(url("https://github.com/jodersky/commando"))
publishMavenStyle in ThisBuild := true
publishTo in ThisBuild := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)
scmInfo in ThisBuild := Some(
  ScmInfo(
    url("https://github.com/jodersky/commando"),
    "scm:git@github.com:jodersky/commando.git"
  )
)
developers in ThisBuild := List(
  Developer(
    id = "jodersky",
    name = "Jakob Odersky",
    email = "jakob@odersky.com",
    url = url("https://crashbox.io")
  )
)
version in ThisBuild := {
  import sys.process._
  ("git describe --always --dirty=-SNAPSHOT --match v[0-9].*" !!).tail.trim
}
