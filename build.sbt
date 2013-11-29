name := "kolibrinet"

version := "0.5"

resolvers += "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(
            jdbc,
            anorm,
           "org.reactivemongo" %% "play2-reactivemongo" % "0.10.0-SNAPSHOT",
           "org.mindrot" % "jbcrypt" % "0.3m",
           "info.schleichardt" %% "play-embed-mongo" % "0.2",
           "com.amazonaws" % "aws-java-sdk" % "1.3.21",
           "javax.mail" % "mail" % "1.4.7"
)     

play.Project.playScalaSettings




