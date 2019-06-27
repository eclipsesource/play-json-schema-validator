// Comment to get more information during initialization
logLevel := Level.Warn

resolvers += Classpaths.sbtPluginReleases

addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.4")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.0")

addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.7")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.11")
