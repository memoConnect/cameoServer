package helper

import constants.ErrorCodes.ErrorCode
import constants.Notifications._
import play.api.Logger
import play.api.libs.json._
import play.api.mvc.{ Results, Result }
import play.api.mvc.Results._

/**
 * User: Björn Reimer
 * Date: 11/11/13
 * Time: 8:51 PM
 */

object ResultHelper {

  val CAMEO_ERROR_CODE = 232
  val noErrorCode: ErrorCode = None

  // OK
  def resOk[A](data: A)(implicit writes: Writes[A], depreciated: Boolean = false): Result = {
    val body = Json.obj(
      "res" -> "OK",
      "data" -> data
    )
    response(Ok, body)
  }

  def resOkWithCache(data: Array[Byte], etag: String): Result =
    addCacheHeaders(Ok(data), etag, None)

  def resOkWithCache(data: Array[Byte], etag: String, fileType: String): Result =
    addCacheHeaders(Ok(data), etag, Some(fileType))

  def resNotModified(): Result = NotModified

  // OK but could not fullfill request
  def resKo[A](data: A, errorCode: ErrorCode = noErrorCode)(implicit writes: Writes[A], depreciated: Boolean = false): Result = {
    val body = Json.obj(
      "res" -> "KO",
      "data" -> data
    ) ++
      JsonHelper.maybeEmptyJson("errorCode", errorCode)

    response(Status(CAMEO_ERROR_CODE), body)
  }

  // Bad Request
  def resBadRequest(error: String, errorCode: ErrorCode = noErrorCode, data: Option[JsObject] = None)(implicit depreciated: Boolean = false): Result = {
    val body = Json.obj(
      "res" -> "KO",
      "error" -> error
    ) ++
      JsonHelper.maybeEmptyJson("data", data) ++
      JsonHelper.maybeEmptyJson("errorCode", errorCode)

    response(BadRequest, body)
  }

  // NotFound
  def resNotFound(what: String)(implicit depreciated: Boolean = false): Result = {
    val body = Json.obj(
      "res" -> "KO",
      "error" -> (what + " not found")
    )
    response(NotFound, body)
  }

  // Not Authorized
  def resUnauthorized(error: String, twoFactorRequired: Boolean = false)(implicit depreciated: Boolean = false): Result = {
    val twoFactor = twoFactorRequired match {
      case false => Json.obj()
      case true  => Json.obj("twoFactorRequired" -> true)
    }
    val body = Json.obj(
      "res" -> "KO",
      "error" -> error)

    response(Unauthorized, body ++ twoFactor)
  }

  // Server Error
  def resServerError(error: String)(implicit depreciated: Boolean = false): Result = {
    val body = Json.obj(
      "res" -> "KO",
      "error" -> error
    )

    response(InternalServerError, body)
  }

  def response(status: Results.Status, body: JsObject)(implicit depreciated: Boolean = false): Result = {

    val depreciatedJs = depreciated match {
      case false => Json.obj()
      case true =>
        Logger.warn("Deprecated call used")
        Json.obj("depreciated" -> true)
    }

    status(body ++ depreciatedJs)
  }

  // cache helpers
  // 5 weeks
  val expire = 60 * 60 * 24 * 7 * 5
  def addCacheHeaders(result: Result, etag: String, fileType: Option[String]): Result = {
    fileType match {
      case None =>
        result
          .withHeaders(("ETAG", etag))
          .withHeaders(("Cache-Control", "max-age=" + expire))
      case Some(ft) =>
        result
          .withHeaders(("ETAG", etag))
          .withHeaders(("Cache-Control", "max-age=" + expire))
          .withHeaders(("Content-Type", ft))
    }
  }

}