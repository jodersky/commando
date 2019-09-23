import mill._, scalalib._, scalafmt._, scalalib.publish._

object commando extends ScalaModule with ScalafmtModule with PublishModule {

  def scalaVersion = T.input {
    sys.props.toMap.get("scala.version") match {
      case Some(v) => v
      case None => "2.13.1"
    }
  }

  object test extends Tests {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.7.1"
    )
    def testFrameworks = Seq("utest.runner.Framework")
  }

  def publishVersion = T.input{os.proc("git", "describe", "--dirty", "--match=v*").call().out.trim.tail}
  def pomSettings = PomSettings(
    description = "Simple command line parsing.",
    organization = "io.crashbox",
    url = "https://github.com/jodersky/commando",
    licenses = Seq(License.`BSD-3-Clause`),
    versionControl = VersionControl.github("jodersky", "commando"),
    developers = Seq(
      Developer("jodersky", "Jakob Odersky","https://github.com/jodersky")
    )
  )

}
