package controllers

import play.api.libs.json.{ JsValue, Json }
import traits.{ ExtendedController }
import models.Conversation
import play.api.libs.concurrent.Execution.Implicits._
import helper.{ CheckHelper, AuthAction }
import helper.ResultHelper._
import play.api.mvc.Action
import com.google.i18n.phonenumbers.PhoneNumberUtil.PhoneNumberFormat
import scala.Some
import scala.Some

/**
 * User: Michael Merz
 * Date: 31/01/14
 * Time: 4:30 PM
 */

object ServicesController extends ExtendedController {

  def checkPhoneNumber = Action(parse.tolerantJson) {
    request =>
      val jsBody: JsValue = request.body
      (jsBody \ "phoneNumber").asOpt[String] match {
        case Some(phoneNumber) => CheckHelper.checkAndCleanPhoneNumber(phoneNumber) match {
          case None    => resBadRequest("invalid phone number")
          case Some(p) => resOK(Json.obj("phoneNumber" -> p))
        }
        case None => resBadRequest("no phoneNumber")
      }
  }

  def checkEmailAddress = Action(parse.tolerantJson) {
    request =>
      val jsBody: JsValue = request.body
      (jsBody \ "emailAddress").asOpt[String] match {
        case Some(email) => CheckHelper.checkAndCleanEmailAddress(email) match {
          case None    => resBadRequest("invalid emailAddress")
          case Some(e) => resOK(Json.obj("email" -> e))
        }
        case None => resBadRequest("missing emailAddress")
      }
  }
}
