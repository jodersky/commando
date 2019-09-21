import mill._, scalalib._, scalafmt._

object commando extends ScalaModule with ScalafmtModule {
  override def scalaVersion = "2.13.0"

  object test extends Tests {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.7.1"
    )
    def testFrameworks = Seq("utest.runner.Framework")
  }

}
