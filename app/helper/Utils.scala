package helper

import java.math.BigInteger

import play.api.Play
import play.api.Play.current

import scala.sys.process.stringSeqToProcess

/**
 * User: Björn Reimer
 * Date: 2/19/14
 * Time: 12:14 PM
 */
object Utils {

  def safeStringToInt(str: String): Option[Int] =
    try {
      Some(str.toInt)
    } catch {
      case e: NumberFormatException =>
        None
    }

  def getAppVersion: String = {
    // see if there is an app version in the config, else try to get it from git
    Play.configuration.getString("application.version").getOrElse({
      val tag: String = Seq("bash", "-c", "git describe --abbrev=0 --tags").!!.trim
      val branch: String = Seq("bash", "-c", "git rev-parse --abbrev-ref HEAD").!!.trim
      branch match {
        case "dev" => "dev_build." + tag.split('_')(1)
        case b     => b
      }
    })
  }

  // hash helper
  private def md5BigInteger(s: String): BigInteger = {
    val m = java.security.MessageDigest.getInstance("MD5")
    val b = s.getBytes("UTF-8")
    m.update(b, 0, b.length)
    new java.math.BigInteger(1, m.digest())
  }

  def md5(s: String): String = {
    md5BigInteger(s).toString(16)
  }

  def md5Long(s: String): Long = {
    md5BigInteger(s).longValue()
  }

}
