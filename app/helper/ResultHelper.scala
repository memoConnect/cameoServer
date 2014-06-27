package helper

import constants.Notifications._
import play.api.libs.json.{ JsObject, _ }
import play.api.mvc.Result
import play.api.mvc.Results._

/**
 * User: Björn Reimer
 * Date: 11/11/13
 * Time: 8:51 PM
 */

object ResultHelper {

  val CAMEO_ERROR_CODE = 232

  // OK
  def resOk(): Result = Ok(Json.obj("res" -> "OK"))

  def resOk(data: JsValue): Result =
    Ok(Json.obj("res" -> "OK") ++
      Json.obj("data" -> data))
  //      ++
  //      addMessagesOrEmpty(notifications))

  def resOk(data: Seq[JsValue]): Result =
    Ok(Json.obj("res" -> "OK") ++
      Json.obj("data" -> data))
  //      ++
  //      addMessagesOrEmpty(notifications))

  def resOk(data: String): Result = Ok(Json.obj("res" -> "OK") ++ Json.obj("data" -> data))

  // 5 weeks
  val expire = 60 * 60 * 24 * 7 * 5
  def resOKWithCache(data: Array[Byte], etag: String): Result =
    Ok(data)
      .withHeaders(("ETAG", etag))
      .withHeaders(("Cache-Control", "max-age=" + expire))

  def resNotModified(): Result = NotModified

  // OK but could not fullfill request
  def resKO(data: JsValue): Result =
    Status(232)(Json.obj("res" -> "KO")
      ++ Json.obj("data" -> data))
  //      ++
  //      addMessagesOrEmpty(notifications))

  def resKO(): Result =
    Status(232)(Json.obj("res" -> "KO"))
  //      ++
  //      addMessagesOrEmpty(notifications))

  def resKo(error: String): Result =
    Status(232)(Json.obj("res" -> "KO")
      ++ Json.obj("error" -> error))
  //      ++
  //      addMessagesOrEmpty(notifications))

  def resKO(notifications: Seq[UserNotification] = Seq()): Result =
    Status(232)(
      Json.obj("res" -> "KO") ++
        addMessagesOrEmpty(notifications))

  // Bad Request
  def resBadRequest(error: String): Result = {
    BadRequest(
      Json.obj("res" -> "KO") ++
        Json.obj("error" -> error))
  }

  def resBadRequest(error: JsObject): Result = {
    BadRequest(
      Json.obj("res" -> "KO") ++
        Json.obj("error" -> error))
  }

  def resBadRequest(notifications: Seq[UserNotification] = Seq()): Result =
    BadRequest(
      Json.obj("res" -> "KO") ++
        addMessagesOrEmpty(notifications))

  // NotFound
  def resNotFound(what: String, notifications: Seq[UserNotification] = Seq()): Result = {
    NotFound(
      Json.obj("res" -> "KO") ++
        Json.obj("error" -> (what + " not found")) ++
        addMessagesOrEmpty(notifications)
    )
  }

  // Not Authorized
  def resUnauthorized(): Result = {
    Unauthorized(Json.obj("res" -> "KO"))
  }

  def resUnauthorized(error: String, twoFactorRequired: Boolean = false): Result = {
    val add = twoFactorRequired match {
      case false => Json.obj()
      case true  => Json.obj("twoFactorRequired" -> true)
    }

    Unauthorized(
      Json.obj("res" -> "KO") ++
        Json.obj("error" -> error) ++
        add)
  }

  // Server Error
  def resServerError(error: String): Result = {
    InternalServerError(
      Json.obj("res" -> "KO") ++ Json.obj("error" -> error)
    )
  }

  def addMessagesOrEmpty(messages: Seq[UserNotification]): JsObject = {
    if (messages.isEmpty) {
      Json.obj()
    } else {
      Json.obj("messages" -> Json.toJson(messages))
    }
  }

  def infoNotify(text: String): Seq[UserNotification] =
    Seq(new UserNotification(USER_MESSAGE_LEVEL_INFO, text))

  def warnNotify(text: String): Seq[UserNotification] =
    Seq(new UserNotification(USER_MESSAGE_LEVEL_WARN, text))

  def errorNotify(text: String): Seq[UserNotification] =
    Seq(new UserNotification(USER_MESSAGE_LEVEL_ERROR, text))
}

// a message directly to the frontend
case class UserNotification(
  severity: String,
  text: String)

object UserNotification {
  implicit val format: Format[UserNotification] = Json.format[UserNotification]
}
