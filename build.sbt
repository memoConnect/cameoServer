import sys.process.stringSeqToProcess

name := "cameoServer"

resolvers += "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.2",
  "org.mindrot" % "jbcrypt" % "0.3m",
  "info.schleichardt" %% "play-embed-mongo" % "0.2",
  "com.amazonaws" % "aws-java-sdk" % "1.3.21",
  "javax.mail" % "mail" % "1.4.7",
  "org.specs2" %% "specs2" % "2.3.7" % "test",
  "com.googlecode.libphonenumber" % "libphonenumber" % "5.9"
)

play.Project.playScalaSettings

com.typesafe.sbt.SbtAtmosPlay.atmosPlaySettings