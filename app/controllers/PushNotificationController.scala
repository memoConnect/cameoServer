package controllers

import models.{ PushDevice, MongoId, Account }
import play.api.Logger
import play.api.libs.json.Json
import traits.ExtendedController
import helper.CmActions.AuthAction
import helper.ResultHelper._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 02.09.14
 * Time: 17:27
 */
object PushNotificationController extends ExtendedController {

  def addPushDevice() = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, PushDevice.createReads) {
        pushDevice =>
          request.identity.accountId match {
            case None => Future(resBadRequest("identity has no account"))
            case Some(accountId) =>
              // check if another account has this deviceId
              PushDevice.findParent(pushDevice.deviceId)
                .flatMap {
                  case Some(account) if !account.id.equals(accountId) =>
                    PushDevice.delete(account.id, pushDevice.deviceId).map(_.updatedExisting)
                  case _ => Future(true)
                }
                .flatMap {
                  case false => Future(false)
                  case true  =>
                    PushDevice.appendOrUpdate(accountId, pushDevice).map(_.updatedExisting)
                }
                .map {
                  case false => resBadRequest("could not add push device")
                  case true  => resOk()
                }
          }
      }
  }

  def deletePushDevice(id: String) = AuthAction().async {
    request =>
      request.identity.accountId match {
        case None => Future(resBadRequest("identity has no account"))
        case Some(accountId) =>
          PushDevice.delete(accountId, MongoId(id)).map(_.updatedExisting).map {
            case false => resBadRequest("could not delete")
            case true  => resOk()
          }
      }
  }
}

