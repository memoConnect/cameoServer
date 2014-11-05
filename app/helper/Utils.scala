package helper

import java.math.BigInteger

import play.api.Play
import play.api.Play.current

import scala.annotation.tailrec
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
        case "dev" => "dev_build." + tag.split('_').lastOption.getOrElse("moep")
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

  case class InvalidVersionException(msg: String) extends RuntimeException

  // returns true if b is a higher than or equal to a.
  def compareVersions(versionA: String, versionB: String): Boolean = {
    val aInt = versionA.split('.').toList.map(safeStringToInt(_).getOrElse(throw new InvalidVersionException(versionA)))
    val bInt = versionB.split('.').toList.map(safeStringToInt(_).getOrElse(throw new InvalidVersionException(versionB)))

    if (aInt.isEmpty) throw new InvalidVersionException(versionA)
    if (bInt.isEmpty) throw new InvalidVersionException(versionB)

    @tailrec
    def check(listA: List[Int], listB: List[Int]): Boolean = {
      (listA, listB) match {
        case (Nil, Nil)                                         => true
        case (Nil, _)                                           => true
        case (_, Nil)                                           => false
        case (headA :: restA, headB :: restB) if headA < headB  => true
        case (headA :: restA, headB :: restB) if headA == headB => check(restA, restB)
        case _                                                  => false
      }
    }
    check(aInt, bInt)
  }

}
