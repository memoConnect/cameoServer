name := "cameoServer"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.10.4"

resolvers += "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.akka23-SNAPSHOT",
  "org.mindrot" % "jbcrypt" % "0.3m",
  "info.schleichardt" %% "play-2-embed-mongo" % "0.5.0",
  "com.amazonaws" % "aws-java-sdk" % "1.7.8.1",
  "javax.mail" % "mail" % "1.4.7",
  "org.specs2" %% "specs2" % "2.3.7" % "test",
  "com.googlecode.libphonenumber" % "libphonenumber" % "5.9",
  "batik" % "batik-svggen" % "1.6-1",
  "batik" % "batik-transcoder" % "1.6-1"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

// disable reverse route to get rid of some useless compiler warnings
//generateReverseRouter := false
