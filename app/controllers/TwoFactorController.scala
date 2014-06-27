package controllers

import helper.CmActions.AuthAction
import helper.ResultHelper._
import models.TwoFactorToken
import play.api.libs.json.{ Json, Reads }
import services.TwoFactorAuth
import traits.ExtendedController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ ExecutionContext, Future }

/**
 * User: Björn Reimer
 * Date: 3/13/14
 * Time: 12:52 PM
 */
object TwoFactorController extends ExtendedController {

  def initiate() = AuthAction().async {
    request =>
      TwoFactorAuth.sendNewKey(request.identity) match {
        case None        => Future(resOk())
        case Some(error) => Future(resBadRequest(error))
      }
  }

  case class ConfirmKey(key: String)

  object ConfirmKey { implicit val reads: Reads[ConfirmKey] = Json.reads[ConfirmKey] }

  def confirmKey() = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, ConfirmKey.reads) { ck =>
        TwoFactorAuth.verifyKey(ck.key, request.identity).map {
          case false => BadRequest("invalid key")
          case true =>
            val newToken = TwoFactorToken.createAndInsert(request.identity.id)
            resOk(newToken.toJson)
        }
      }
  }

}
