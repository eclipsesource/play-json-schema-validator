
import bintray.Plugin._
import sbt.Keys._
import sbt._
import sbtrelease.ReleasePlugin
import sbtrelease.ReleasePlugin._

object Version {
  val play          = "2.4.2"
  val playTest      = "2.4.2"
  val scalaz        = "7.1.3"
  val scalameter    = "0.6"
  val specs2        = "2.4.9"
  val spray         = "1.3.2"
  val akka          = "2.3.6"
  val scalaLogging  = "3.1.0"
  val shapeless     = "2.1.0"
  val jtoValidationCore = "1.0-1c770f4"
  val jtoValidationJson = "1.0-1c770f4"
}

object Library {
  val scalaz        = "org.scalaz"        %% "scalaz-core"            % Version.scalaz
  val play          = "com.typesafe.play" %% "play"                   % Version.play
  val playJson      = "com.typesafe.play" %% "play-json"              % Version.play
  val playWs        = "com.typesafe.play" %% "play-ws"                % Version.play
  val scalaLogging  = "com.typesafe.scala-logging" %% "scala-logging" % Version.scalaLogging
  val playTest      = "com.typesafe.play" %% "play-specs2"            % Version.play           % "test"
  val scalameter    = "com.storm-enroute" %% "scalameter"             % Version.scalameter     % "test"
  val scalaXml      =  "org.scala-lang.modules" %% "scala-xml" % "1.0.3"
  val shapeless     = "com.chuusai"       %% "shapeless"              % Version.shapeless
  val specs2        = "org.specs2"        %% "specs2"                 % Version.specs2         % "test"
  val jtoValidationCore = "io.github.jto" %% "validation-core"        % Version.jtoValidationCore
  val jtoValidationJson = "io.github.jto" %% "validation-json"        % Version.jtoValidationJson
}

object Dependencies {
  import Library._

  val core = List(
    playTest,
    playJson,
    scalaz,
    scalameter,
    shapeless,
    specs2,
    jtoValidationCore,
    jtoValidationJson
  )
}

object Build extends Build {

  val Repositories = Seq(
    "Typesafe repository"           at "http://repo.typesafe.com/typesafe/releases/",
    "Sonatype OSS Snapshots"        at "https://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype OSS Releases"         at "https://oss.sonatype.org/content/repositories/releases",
    "Mandubian repository releases" at "https://github.com/mandubian/mandubian-mvn/tree/master/releases",
    "JTO snapshots"                 at "https://raw.github.com/jto/mvn-repo/master/snapshots",
    "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
  )

  val commonSettings = Seq(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*) ++
    Seq(
      organization := "com.eclipsesource",
      scalaVersion := "2.11.6",
      crossScalaVersions := Seq("2.10.4","2.11.6"),
      scalacOptions += "-target:jvm-1.8",
      licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
      Keys.fork in Test := false,
      Keys.parallelExecution in Test := false
    )

  val releaseSettings = ReleasePlugin.releaseSettings ++ bintrayPublishSettings ++ Seq(
    publishMavenStyle := true,
    publishTo := (publishTo in bintray.Keys.bintray).value, // set it globally so that sbt-release plugin does not freak out.
    bintray.Keys.bintrayOrganization in bintray.Keys.bintray := None,
    bintray.Keys.packageLabels in bintray.Keys.bintray := Seq("json", "json-schema", "play", "scala"),
    ReleaseKeys.crossBuild := true
  )

  val buildSettings = Defaults.coreDefaultSettings ++ commonSettings

  lazy val schemaProject = Project("play-json-schema-validator", file("."))
    .settings(buildSettings: _*)
    .settings(releaseSettings: _*)
    .settings(
      resolvers ++= Repositories,
      retrieveManaged := true,
      libraryDependencies ++= Dependencies.core,
      testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
    )

}