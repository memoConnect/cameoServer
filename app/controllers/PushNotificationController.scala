package controllers

import helper.AuthenticationActions.AuthAction
import helper.ResultHelper._
import models.{ MongoId }
import play.api.libs.json.Json
import traits.ExtendedController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 02.09.14
 * Time: 17:27
 */
object PushNotificationController extends ExtendedController {

  case class AddPushDevice(deviceToken: String, platform: String, language: String)
  object AddPushDevice {implicit val format = Json.format[AddPushDevice]}

  def addPushDevice() = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, AddPushDevice.format) {
        pushDevice =>
          Future(resOk(Json.toJson(pushDevice)))

//          request.identity.accountId match {
//            case None => Future(resBadRequest("identity has no account"))
//            case Some(accountId) =>
//              // check if another account has this deviceId
//              PushDevice.findParent(pushDevice.deviceId)
//                .flatMap {
//                  case Some(account) if !account.id.equals(accountId) =>
//                    PushDevice.delete(account.id, pushDevice.deviceId).map(_.updatedExisting)
//                  case _ => Future(true)
//                }
//                .flatMap {
//                  case false => Future(false)
//                  case true =>
//                    PushDevice.appendOrUpdate(accountId, pushDevice).map(_.updatedExisting)
//                }
//                .map {
//                  case false => resBadRequest("could not add push device")
//                  case true  => resOk()
//                }
//          }
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

