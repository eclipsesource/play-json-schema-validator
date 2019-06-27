import sbt._

object Version {
  final val playJson      = "2.7.4"
  final val playSpecs     = "2.7.3"
  final val scalaz        = "7.2.27"
  final val specs2        = "4.5.1"
  final val guava         = "19.0"
  final val i18n          = "1.0.3"
  final val galimatias    = "0.2.1"
}

object Library {
  final val guava         = "com.google.guava"   % "guava"                   % Version.guava
  final val scalaz        = "org.scalaz"         %% "scalaz-core"            % Version.scalaz
  final val playJson      = "com.typesafe.play"  %% "play-json"              % Version.playJson
  final val playTest      = "com.typesafe.play"  %% "play-specs2"            % Version.playSpecs      % "test"
  final val specs2        = "org.specs2"         %% "specs2-core"            % Version.specs2         % "test"
  final val i18n          = "com.osinka.i18n"    %% "scala-i18n"             % Version.i18n
  final val galimatias    = "io.mola.galimatias" % "galimatias"              % Version.galimatias
}


object Dependencies {
  import Library._

  val core = List(
    galimatias,
    guava,
    i18n,
    playJson,
    playTest,
    scalaz,
    specs2
  )
}