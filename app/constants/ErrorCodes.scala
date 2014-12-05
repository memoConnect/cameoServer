package constants

/**
 * User: Björn Reimer
 * Date: 30.10.14
 * Time: 14:45
 */

object ErrorCodes {

  type ErrorCode = String

  val FILE_UPLOAD_QUOTA_EXCEEDED: ErrorCode = "FILE.UPLOAD.QUOTA.EXCEEDED"
  val FILE_UPLOAD_FILESIZE_EXCEEDED: ErrorCode = "FILE.UPLOAD.FILESIZE.EXCEEDED"

  val VERIFY_EXPIRED: ErrorCode = "VERIFY.EXPIRED"
  val VERIFY_VALUE_CHANGED: ErrorCode = "VERIFY.VALUE.CHANGED"

  val PASSWORD_RESET_EMAIL_NOT_FOUND: ErrorCode = "PASSWORD.RESET.EMAIL.NOT.FOUND"
  val PASSWORD_RESET_PHONENUMBER_NOT_FOUND: ErrorCode = "PASSWORD.RESET.PHONENUMBER.NOT.FOUND"
  val PASSWORD_RESET_LOGIN_NOT_FOUND: ErrorCode = "PASSWORD.RESET.LOGIN.NOT.FOUND"
  val PASSWORD_RESET_EXPIRED: ErrorCode = "PASSWORD.RESET.EXPIRED"

}
