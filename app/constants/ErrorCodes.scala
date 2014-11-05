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

}
