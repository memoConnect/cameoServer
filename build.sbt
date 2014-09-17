import play.PlayScala

name := "cameoServer"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  cache,
  ws,
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.akka23-SNAPSHOT",
  "org.mindrot" % "jbcrypt" % "0.3m",
  "de.flapdoodle.embed" % "de.flapdoodle.embed.mongo" % "1.46.0",
  "com.amazonaws" % "aws-java-sdk" % "1.7.8.1",
  "javax.mail" % "mail" % "1.4.7",
  "com.googlecode.libphonenumber" % "libphonenumber" % "5.9",
  "batik" % "batik-svggen" % "1.6-1",
  "batik" % "batik-transcoder" % "1.6-1",
  "com.typesafe.play.plugins" %% "play-statsd" % "2.3.0",
  "org.imgscalr" % "imgscalr-lib" % "4.2", // image scaler
  "org.json" % "org.json" % "chargebee-1.0", // needed of puship API
  "net.sf.uadetector" % "distribution" % "2014.08"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
