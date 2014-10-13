package helper

import com.google.i18n.phonenumbers.PhoneNumberUtil
import com.google.i18n.phonenumbers.PhoneNumberUtil.PhoneNumberFormat

/**
 * User: Björn Reimer
 * Date: 3/6/14
 * Time: 5:36 PM
 */
object CheckHelper {

  def checkAndCleanPhoneNumber(phoneNumber: String): Option[String] = {
    val trimmed = phoneNumber.trim
    // check whether it consist of numbers and "/-+"
    val valid = "0123456789+/- ()"
    trimmed.forall(char => valid.contains(char)) match {
      case false => None
      case true =>
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

  }

  def checkAndCleanEmailAddress(email: String): Option[String] = {
    val trimmed = email.trim
    """^[a-zA-Z0-9.\-_]+@[a-zA-Z0-9.\-_]+\.[a-zA-Z][a-zA-Z]+$""".r.unapplySeq(trimmed).isDefined match {
      case true  => Some(trimmed)
      case false => None
    }
  }

  // Left: phonenumber, Right: email
  def checkAndCleanMixed(mixed: String): Option[Either[String, String]] = {
    val maybeTel = CheckHelper.checkAndCleanPhoneNumber(mixed)
    val maybeMail = CheckHelper.checkAndCleanEmailAddress(mixed)
    (maybeTel, maybeMail) match {
      case (None, None)        => None
      case (Some(tel), None)   => Some(Left(tel))
      case (None, Some(email)) => Some(Right(email))
      case _                   => None
    }
  }
}
