package controllers

import play.api.libs.json.{JsValue, Json}
import traits.{ExtendedController}
import models.{Conversation, Purl}
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import helper.AuthAction
import scala.concurrent.Future
import helper.ResultHelper._
import play.api.mvc.Action
import com.google.i18n.phonenumbers.PhoneNumberUtil
import com.google.i18n.phonenumbers.PhoneNumberUtil.PhoneNumberFormat
import scala.Some
import scala.Some

/**
 * User: Michael Merz
 * Date: 31/01/14
 * Time: 4:30 PM
 */

object ServicesController extends ExtendedController {

  /**
   * Actions
   */
  def checkPhoneNumber() = Action(parse.tolerantJson) {
    request =>
      val jsBody: JsValue = request.body
      (jsBody \ "phoneNumber").asOpt[String] match {
        case Some(phoneNumber) =>
          try {
            val phoneUtil = PhoneNumberUtil.getInstance()
            // default country code have to be a property
            val number = phoneUtil.parseAndKeepRawInput(phoneNumber, "DE")
            val resultJson = Json.obj(
              "status" -> "ok",
              "phoneNumber" -> phoneUtil.format(number, PhoneNumberFormat.E164))
            resOK(resultJson)
          }
          catch {
            case e: Exception => {
              Logger.error("phoneNumber parse error: " + phoneNumber)
              BadRequest(resKO(e.getMessage + " > " + phoneNumber))
            }
          }
        case None => BadRequest(resKO("missing phoneNumber"))
      }
  }
}
