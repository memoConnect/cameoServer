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

  val MESSAGE_SMS_REPLACE_ENCRYPTED = "I have send you an encrypted message via cameoNet.de. Click here to read the message and answer: "
  val MESSAGE_MAIL_REPLACE_ENCRYPTED_DE = "Ich habe Ihnen eine verschlüsselte Nachricht über cameoNet.de gesendet.\nBitte klicken Sie auf folgenden Link um den gesamten Talk zu lesen und zu antworten: "
  val MESSAGE_MAIL_REPLACE_ENCRYPTED_EN = "I have send you an encrypted message via cameoNet.de.\nPlease click the following link to read it and answer: "

  val MESSAGE_SMS_FILE_ONLY = "I have send you a file via cameoNet.de. Click here to download it: "
  val MESSAGE_MAIL_FILE_ONLY_DE = "Ich habe Ihnen eine Datei über cameoNet.de gesendet.\nBitte klicken Sie auf folgenden Link um die Datei zu lesen und zu antworten: "
  val MESSAGE_MAIL_FILE_ONLY_EN = "I have send you a file via cameoNet.de. Click here to download it and awnser: "
}

object Confirmation {
  val CONFIRMATION_TYPE_RESET_PASSWORD = "reset-password"
  val CONFIRMATION_TYPE_VERIFICATION = "verification"

  val CONFIRMATION_PATH_MAIL = "mail"
  val CONFIRMATION_PATH_PHONENUMBER = "phoneNumber"
}

object KeyTransmission {

  val KEY_TRANSMISSION_NONE = "none"
  val KEY_TRANSMISSION_MIXED = "mixed"
  val KEY_TRANSMISSION_SYMMETRIC = "symmetric"
  val KEY_TRANSMISSION_ASYMMECTRIC = "asymmetric"
}
