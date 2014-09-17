package controllers

import helper.AuthenticationActions.AuthAction
import helper.ResultHelper._
import models.{ MongoId }
import play.api.Logger
import play.api.i18n.Lang
import play.api.libs.Crypto
import play.api.libs.json._
import services.PushdConnector
import services.PushdConnector._
import traits.ExtendedController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 02.09.14
 * Time: 17:27
 */
object PushNotificationController extends ExtendedController {

  case class AddPushDevice(deviceToken: String, platform: String, language: Lang)

  object AddPushDevice {

    implicit val langReads: Reads[Lang] = Reads {
      js =>
        js.asOpt[String] match {
          case None => JsError("no language")
          case Some(lang) =>
            try {
              JsSuccess(Lang(lang))
            } catch {
              case e: RuntimeException => JsError("invalid language code")
            }
        }
    }

    implicit val langWrites: Writes[Lang] = Writes {
      lang => JsString(lang.code)
    }

    implicit val format = Json.format[AddPushDevice]
  }

  def addPushDevice() = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, AddPushDevice.format) {
        pushDevice =>

          val platform: PushdPlatform = pushDevice.platform match {
            case "ios" => APNS
            case "and" => GCM
            case "win" => MPNS
          }

          // get id for this token
          PushdConnector.getSubscriberId(pushDevice.deviceToken, platform, pushDevice.language).flatMap {
            case None => Future(resKo("Pushd is down"))
            case Some(id) =>
              // set subscription to identityId of this user
              setSubscriptions(id, Seq(request.identity.id.id)).map {
                case false => resKo("Pushd is down")
                case true => resOk()
              }
          }
      }
  }

  def deletePushDevice(id: String) = AuthAction() {
    request =>
      resOk()

    //      request.identity.accountId match {
    //        case None => Future(resBadRequest("identity has no account"))
    //        case Some(accountId) =>
    //          PushDevice.delete(accountId, MongoId(id)).map(_.updatedExisting).map {
    //            case false => resBadRequest("could not delete")
    //            case true  => resOk()
    //          }
    //      }
  }
}

