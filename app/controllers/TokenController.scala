package controllers

import constants.ErrorCodes
import helper.ResultHelper._
import models.{ Account, Identity, Token }
import org.mindrot.jbcrypt.BCrypt
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.Json
import play.api.mvc._
import services.AuthenticationActions._
import traits.ExtendedController

import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 5/28/13
 * Time: 4:30 PM
 */
object TokenController extends ExtendedController {

  /**
   * Actions
   */
  def createToken() = BasicAuthAction().async {
    request =>
      //get default identity
      Identity.findAll(Json.obj("accountId" -> request.account.id, "isDefaultIdentity" -> true)).flatMap {
        case Seq() => Future(resKo("", ErrorCodes.ACCOUNT_MISSING_IDENTITY))
        case Seq(identity) =>
          val token = Token.createDefault()
          identity.addToken(token).map {
            le => resOk(token.toJson)
          }
        case _ => Future(resServerError("more than one default identity"))
      }
  }

  def getTokenOptions(foo: String) = Action {
    request =>
      Ok("")
  }

  //  def deleteToken(token: String) = AuthAction.async {
  //    request =>
  //      // check if token exists
  //      request.identity
  //
  //
  //      request.identity.deleteToken(new MongoId(token)).map {
  //        lastError =>
  //          if (lastError.updatedExisting) {
  //            resOK("deleted")
  //          } else {
  //            resNotFound("token")
  //          }
  //      }
  //
  //  }

  def getToken(id: String) = AuthAction().async {
    request =>
      request.identity.accountId match {
        case None => Future(resBadRequest("identity has no account"))
        case Some(accountId) =>
          Identity.findByAccountId(accountId).map { list =>
            list.find(_.id.id.equals(id)) match {
              case None => resNotFound("identity within account")
              case Some(identity) =>
                val token = Token.createDefault()
                Identity.addTokenToIdentity(identity.id, token)
                resOk(token.toJson)
            }
          }
      }
  }
}
