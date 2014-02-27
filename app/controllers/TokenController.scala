package controllers

import play.api.mvc._
import play.api.libs.json.{ Json, JsValue }
import scala.concurrent.Future
import org.mindrot.jbcrypt.BCrypt
import traits.ExtendedController
import models.{ MongoId, Identity, Account, Token }
import play.api.libs.concurrent.Execution.Implicits._
import helper.ResultHelper._
import play.api.Logger
import helper.AuthAction

/**
 * User: Björn Reimer
 * Date: 5/28/13
 * Time: 4:30 PM
 */
object TokenController extends ExtendedController {

  /**
   * Helper
   */
  // decode username and password
  def decodeBasicAuth(auth: String) = {
    val baStr = auth.replaceFirst("Basic ", "").replace(" ", "")
    val Array(user, pass) = new String(new sun.misc.BASE64Decoder().decodeBuffer(baStr), "UTF-8").split(":")
    (user, pass)

  }

  /**
   * Actions
   */
  def createToken() = Action.async {
    request =>
      {
        request.headers.get("Authorization") match {
          case None => {
            Future.successful(resBadRequest("No Authorization field in header"))
          }
          case Some(basicAuth) => {
            val (loginName, password) = decodeBasicAuth(basicAuth)
            //find account and get first identity
            Account.findByLoginName(loginName).flatMap {
              case None => Future(resUnauthorized("Invalid password/loginName"))
              case Some(account) => if (account.identities.nonEmpty) {
                val identityId = account.identities(0)

                Identity.find(identityId).map {
                  case None => resNotFound("identity")
                  case Some(identity) => {
                    // check loginNames and passwords match
                    if (BCrypt.checkpw(password, account.password) && account.loginName.equals(loginName)) {
                      // everything is ok
                      val token = Token.create
                      identity.addToken(token)
                      resOK(token.toJson)
                    } else {
                      resUnauthorized("Invalid password/loginName")
                    }
                  }
                }
              } else {
                Future.successful(resUnauthorized("Invalid password/loginName"))
              }
            }
          }
        }
      }
  }

  def getTokenOptions(foo: String) = Action {
    request =>
      Ok("")
  }

  def deleteToken(token: String) = AuthAction.async {
    request =>
      request.identity.deleteToken(new MongoId(token)).map {
        lastError =>
          if (lastError.updatedExisting) {
            resOK("deleted")
          } else {
            resNotFound("token")
          }
      }

  }

}
