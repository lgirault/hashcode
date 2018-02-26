// build.sc
import mill._
import mill.scalalib._

object pizza extends ScalaModule {
  def scalaVersion = "2.12.4"

  object test extends Tests {

    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.4")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}

