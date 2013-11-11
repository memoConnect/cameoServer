import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName = "kolibrinet"
  val appVersion = "0.5"

  val appDependencies = Seq(
    jdbc,
    anorm,
    "org.reactivemongo" %% "play2-reactivemongo" % "0.9" exclude("org.scala-stm", "scala-stm_2.10.0"),
    "org.mindrot" % "jbcrypt" % "0.3m",
    "info.schleichardt" %% "play-embed-mongo" % "0.2",
    "com.amazonaws" % "aws-java-sdk" % "1.3.21",
    "javax.mail" % "mail" % "1.4.7"
  )

  val main = play.Project(appName, appVersion, appDependencies)

}
