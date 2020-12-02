import mill._, scalalib._, scalafmt._

import coursier.maven.MavenRepository

import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`

trait BaseModule extends SbtModule with ScalafmtModule {
  def scalaVersion = "2.13.3"
  def scalacOptions =
    Seq(
      "-deprecation", // Emit warning and location for usages of deprecated APIs.
      "-feature", // Emit warning and location for usages of features that should be imported explicitly.
      "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
      "-language:higherKinds", // Allow higher-kinded types
      "-unchecked", // Enable additional warnings where generated code depends on assumptions.
      "-Xfatal-warnings" // Fail the compilation if there are any warnings.
    )
}

object advent extends BaseModule
