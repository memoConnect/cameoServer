package constants

/**
 * User: Björn Reimer
 * Date: 1/22/14
 * Time: 2:40 PM
 */
object Messaging {

  val MESSAGE_TYPE_SMS = "sms"
  val MESSAGE_TYPE_EMAIL = "email"
  val MESSAGE_TYPE_DEFAULT = "default"

  val MESSAGE_STATUS_NONE = "none"
  val MESSAGE_STATUS_QUEUED = "queued"
  val MESSAGE_STATUS_SEND = "send"
  val MESSAGE_STATUS_CONFIRMED = "confirmed"
  val MESSAGE_STATUS_ERROR = "error"

  val MESSAGE_MAX_TRY_COUNT = 0

  val MESSAGE_TEXT_REPLACE_ENCRYPTED = "-= encrypted Message =-"

}

object Verification {

  val VERIFY_TYPE_MAIL = "mail"
  val VERIFY_TYPE_PHONENUMBER = "phoneNumber"
}

object KeyTransmission {

  val KEY_TRANSMISSION_NONE = "none"
  val KEY_TRANSMISSION_MIXED = "mixed"
  val KEY_TRANSMISSION_SYMMETRIC = "symmetric"
  val KEY_TRANSMISSION_ASYMMECTRIC = "asymmetric"
}
