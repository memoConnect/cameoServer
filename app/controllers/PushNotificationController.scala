package controllers

import models.Account
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

  case class AddDeviceId(deviceId: String)
  object AddDeviceId { implicit val format = Json.format[AddDeviceId] }

  def addDeviceId() = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, AddDeviceId.format) {
        adi =>
          request.identity.accountId match {
            case None => Future(resBadRequest("identity has no account"))
            case Some(accountId) =>
              Account.addDeviceId(accountId, adi.deviceId).map {
                case false => resServerError("could not add deviceId")
                case true  => resOk()
              }
          }
      }
  }

  def deleteDeviceId(id: String) = AuthAction().async {
    request =>
      request.identity.accountId match {
        case None => Future(resBadRequest("identity has no account"))
        case Some(accountId) =>
          Account.find(accountId).flatMap {
            case None => Future(resServerError("account not found"))
            case Some(account) => account.deleteDeviceId(id).map{
              case false => resServerError("could not delete")
              case true => resOk()
            }
          }
      }
  }
}
