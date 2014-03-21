package helper

import play.api.mvc.BodyParsers.parse
import play.api.libs.json.Json
import com.google.i18n.phonenumbers.PhoneNumberUtil
import com.google.i18n.phonenumbers.PhoneNumberUtil.PhoneNumberFormat
import scala.Some

/**
 * User: Björn Reimer
 * Date: 3/6/14
 * Time: 5:36 PM
 */
object CheckHelper {

  def checkAndCleanPhoneNumber(phoneNumber: String): Option[String] = {
    val trimmed = phoneNumber.trim
    val phoneUtil = PhoneNumberUtil.getInstance()
    try {
      // TODO: default country code has to be a property
      val number = phoneUtil.parseAndKeepRawInput(trimmed, "DE")
      Some(phoneUtil.format(number, PhoneNumberFormat.E164))
    } catch {
      case e: Exception => {
        None
      }
    }
  }

  def checkAndCleanEmailAddress(email: String): Option[String] = {
    val trimmed = email.trim
    """^[a-zA-Z0-9.\-_]+@[a-zA-Z0-9.\-_]+\.[a-zA-Z][a-zA-Z]+$""".r.unapplySeq(trimmed).isDefined match {
      case true  => Some(trimmed)
      case false => None
    }
  }
}
