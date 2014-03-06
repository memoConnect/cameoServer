package helper

import java.lang.NumberFormatException
import play.api.{Play, Logger}
import sys.process.stringSeqToProcess
import play.api.Play
import play.api.Play.current

/**
 * User: Björn Reimer
 * Date: 2/19/14
 * Time: 12:14 PM
 */
object General {

  def safeStringToInt(str: String): Option[Int] =
    try {
      Some(str.toInt)
    } catch {
      case e: NumberFormatException =>
        Logger.debug("NOT a number: " + str)
        None
    }

  def getAppVersion: String = {
    // see if there is an app version in the config, else try to get it from git
    Play.configuration.getString("application.version").getOrElse({
      val tag: String = Seq("bash","-c", "git describe --abbrev=0 --tags").!!.trim
      val branch: String = Seq("bash","-c", "git rev-parse --abbrev-ref HEAD").!!.trim
      branch match {
        case "dev" => "dev_build." + tag.split('_')(1)
        case "master" => tag
        case b => b
      }
    })
  }


}
