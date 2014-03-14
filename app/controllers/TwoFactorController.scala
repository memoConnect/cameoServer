package controllers

import traits.ExtendedController
import helper.AuthAction
import services.TwoFactorAuth
import helper.ResultHelper._
import play.api.libs.json.{Json, Reads}
import models.TwoFactorToken
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import play.api.Logger

/**
 * User: Björn Reimer
 * Date: 3/13/14
 * Time: 12:52 PM
 */
object TwoFactorController extends ExtendedController {

  def initiate() = AuthAction {
    request =>
      TwoFactorAuth.sendNewKey(request.identity) match {
      case None => resOK()
      case Some(error) => resBadRequest(error)
    }
  }

  case class ConfirmKey(key: String)

  object ConfirmKey { implicit val reads: Reads[ConfirmKey] = Json.reads[ConfirmKey]}

  def confirmKey() = AuthAction.async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, ConfirmKey.reads) { ck =>
        TwoFactorAuth.verifyKey(ck.key, request.identity).map {
          case false => BadRequest("invalid key")
          case true =>
            val newToken = TwoFactorToken.create(request.identity.id)
            resOK(newToken.toJson)
        }
      }
  }

}
