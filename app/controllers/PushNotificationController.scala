package controllers

import helper.ResultHelper._
import models.Identity
import play.api.i18n.Lang
import play.api.libs.json._
import services.AuthenticationActions.AuthAction
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

  case class AddPushDevice(deviceToken: String, platform: PushdPlatform, language: Lang)

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
    implicit val reads = Json.reads[AddPushDevice]
  }

  def addPushDevice() = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, AddPushDevice.reads) {
        pushDevice =>
          // get id for this token
          PushdConnector.getSubscriberId(pushDevice.deviceToken, pushDevice.platform, pushDevice.language).flatMap {
            case None => Future(resKo("Invalid token or platform"))
            case Some(id) =>
              // set subscription to identityIds of this account
              request.identity.accountId match {
                case None => Future(resBadRequest("no account"))
                case Some(accountId) =>
                  Identity.findByAccountId(accountId).flatMap {
                    identities =>
                      val identityIds = identities.map(_.id.id)
                      setSubscriptions(id, identityIds).map {
                        case false => resKo("could not set subscription")
                        case true  => resOk("added")
                      }
                  }
              }
          }
      }
  }

  def deletePushDevice(id: String, platform: String) = AuthAction().async {
    request =>
      validateFuture(JsString(platform), PushdConnector.platformReads) {
        platform =>
          // get id for this token
          PushdConnector.getSubscriberId(id, platform, Lang("en")).flatMap {
            case None => Future(resKo("Pushd is down"))
            case Some(subscriberId) =>
              // set subscription to identityId of this user
              setSubscriptions(subscriberId, Seq()).map {
                case false => resKo("Pushd is down")
                case true  => resOk("deleted")
              }
          }
      }
  }
}

