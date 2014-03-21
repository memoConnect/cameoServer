package helper

import play.api.mvc.Results
import play.api.mvc.Results._
import play.api.libs.json._
import constants.Notifications._
import play.api.libs.json.JsObject
import play.api.mvc.SimpleResult

/**
 * User: Björn Reimer
 * Date: 11/11/13
 * Time: 8:51 PM
 */

object ResultHelper {

  val CAMEO_ERROR_CODE = 232

  // OK
  def resOK(): SimpleResult = Ok(Json.obj("res" -> "OK"))

  def resOK(data: JsValue): SimpleResult =
    Ok(Json.obj("res" -> "OK") ++
      Json.obj("data" -> data))
  //      ++
  //      addMessagesOrEmpty(notifications))

  def resOK(data: Seq[JsValue]): SimpleResult =
    Ok(Json.obj("res" -> "OK") ++
      Json.obj("data" -> data))
  //      ++
  //      addMessagesOrEmpty(notifications))

  def resOK(data: String): SimpleResult = Ok(Json.obj("res" -> "OK") ++ Json.obj("data" -> data))

  // OK but could not fullfill request
  def resKO(data: JsValue): SimpleResult =
    Status(232)(Json.obj("res" -> "KO")
      ++ Json.obj("data" -> data))
  //      ++
  //      addMessagesOrEmpty(notifications))

  def resKO(): SimpleResult =
    Status(232)(Json.obj("res" -> "KO"))
  //      ++
  //      addMessagesOrEmpty(notifications))

  def resKO(error: String): SimpleResult =
    Status(232)(Json.obj("res" -> "KO")
      ++ Json.obj("error" -> error))
  //      ++
  //      addMessagesOrEmpty(notifications))

  def resKO(notifications: Seq[UserNotification] = Seq()): SimpleResult =
    Status(232)(
      Json.obj("res" -> "KO") ++
        addMessagesOrEmpty(notifications))

  // Bad Request
  def resBadRequest(error: String): SimpleResult = {
    BadRequest(
      Json.obj("res" -> "KO") ++
        Json.obj("error" -> error))
    //        ++
    //        addMessagesOrEmpty(notifications))

  }
  def resBadRequest(notifications: Seq[UserNotification] = Seq()): SimpleResult =
    BadRequest(
      Json.obj("res" -> "KO") ++
        addMessagesOrEmpty(notifications))

  // NotFound
  def resNotFound(what: String, notifications: Seq[UserNotification] = Seq()): SimpleResult = {
    NotFound(
      Json.obj("res" -> "KO") ++
        Json.obj("error" -> (what + " not found")) ++
        addMessagesOrEmpty(notifications)
    )
  }

  // Not Authorized
  def resUnauthorized(): SimpleResult = {
    Unauthorized(Json.obj("res" -> "KO"))
  }

  def resUnauthorized(error: String, twoFactorRequired: Boolean = false): SimpleResult = {
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
  def resServerError(error: String): SimpleResult = {
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
