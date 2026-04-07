import mill._
import mill.scalalib._

object myproject extends ScalaModule {
  def scalaVersion = "2.13.10"

  // Chisel 3.5.6 dependencies
  def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.5.6"
  )

  def scalacPluginIvyDeps = Agg(
    ivy"edu.berkeley.cs:::chisel3-plugin:3.5.6"
  )

  object test extends ScalaTests {
    def ivyDeps = Agg(
      ivy"edu.berkeley.cs::chiseltest:0.5.6",
      ivy"org.scalatest::scalatest:3.2.16"
    )
    def testFramework = "org.scalatest.tools.Framework"
  }
}