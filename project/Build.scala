
import sbt.Keys._
import sbt._
import sbtrelease.ReleasePlugin
import sbtrelease.ReleasePlugin._

object Version {
  val play          = "2.4.2"
  val playTest      = "2.4.2"
  val scalaz        = "7.2.0"
  val scalaXml      = "1.0.3"
  val specs2        = "3.7.1"
  val jtoValidationCore = "1.1"
  val jtoValidationJson = "1.1"
  val guava         = "19.0"
}

object Library {
  val guava         = "com.google.guava"  % "guava"                   % Version.guava
  val scalaz        = "org.scalaz"        %% "scalaz-core"            % Version.scalaz
  val play          = "com.typesafe.play" %% "play"                   % Version.play
  val playJson      = "com.typesafe.play" %% "play-json"              % Version.play
  val playWs        = "com.typesafe.play" %% "play-ws"                % Version.play
  val scalaXml      = "org.scala-lang.modules" %% "scala-xml" % "1.0.3"
  val jtoValidationCore = "io.github.jto" %% "validation-core"        % Version.jtoValidationCore
  val jtoValidationJson = "io.github.jto" %% "validation-json"        % Version.jtoValidationJson
  val playTest      = "com.typesafe.play" %% "play-specs2"            % Version.play           % "test"
  val specs2        = "org.specs2"        %% "specs2-core"            % Version.specs2         % "test"
}

object Dependencies {
  import Library._

  val core = List(
    playTest,
    playJson,
    scalaz,
    specs2,
    guava,
    jtoValidationCore,
    jtoValidationJson
  )
}

object Build extends Build {

  import bintray.BintrayKeys._

  val Repositories = Seq(
    "Typesafe repository"           at "http://repo.typesafe.com/typesafe/releases/",
    "Sonatype OSS Snapshots"        at "https://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype OSS Releases"         at "https://oss.sonatype.org/content/repositories/releases",
    "Mandubian repository releases" at "https://github.com/mandubian/mandubian-mvn/tree/master/releases",
    "JTO snapshots"                 at "https://raw.github.com/jto/mvn-repo/master/snapshots",
    "scalaz-bintray"                at "http://dl.bintray.com/scalaz/releases"
  )

  val commonSettings = Seq(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*) ++
    Seq(
      organization := "com.eclipsesource",
      scalaVersion := "2.11.6",
      crossScalaVersions := Seq("2.10.6", "2.11.6"),
      licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
      Keys.fork in Test := false,
      Keys.parallelExecution in Test := false
    )

  val releaseSettings = ReleasePlugin.releaseSettings ++ Seq(
    publishMavenStyle := true,
    bintrayOrganization := None,
    bintrayPackageLabels := Seq("json", "json-schema", "play", "scala"),
    bintrayVcsUrl := Some("git@github.com:eclipsesource/play-json-schema-validator.git"),
    ReleaseKeys.crossBuild := true
  )

  val buildSettings = Defaults.coreDefaultSettings ++ commonSettings

  lazy val schemaProject = Project("play-json-schema-validator", file("."))
    .settings(buildSettings)
    .settings(releaseSettings: _*)
    .settings(
      resolvers ++= Repositories,
      retrieveManaged := true,
      libraryDependencies ++= Dependencies.core,
      testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
    )

}