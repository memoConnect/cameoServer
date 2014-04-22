package controllers

import traits.ExtendedController
import helper.AuthRequestHelper.authAction
import services.TwoFactorAuth
import helper.ResultHelper._
import play.api.libs.json.{ Json, Reads }
import models.TwoFactorToken
import scala.concurrent.{Future, ExecutionContext}
import ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 3/13/14
 * Time: 12:52 PM
 */
object TwoFactorController extends ExtendedController {

  def initiate() = authAction().async {
    request =>
      TwoFactorAuth.sendNewKey(request.identity) match {
        case None        => Future(resOK())
        case Some(error) => Future(resBadRequest(error))
      }
  }

  case class ConfirmKey(key: String)

  object ConfirmKey { implicit val reads: Reads[ConfirmKey] = Json.reads[ConfirmKey] }

  def confirmKey() = authAction().async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, ConfirmKey.reads) { ck =>
        TwoFactorAuth.verifyKey(ck.key, request.identity).map {
          case false => BadRequest("invalid key")
          case true =>
            val newToken = TwoFactorToken.createAndInsert(request.identity.id)
            resOK(newToken.toJson)
        }
      }
  }

}
