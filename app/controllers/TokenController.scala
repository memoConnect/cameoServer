package controllers

import helper.ResultHelper._
import models.{ Account, Identity, Token }
import org.mindrot.jbcrypt.BCrypt
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc._
import traits.ExtendedController

import scala.concurrent.Future

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
  def decodeBasicAuth(auth: String): (String, String) = {
    val baStr = auth.replaceFirst("Basic ", "").replace(" ", "")
    new String(new sun.misc.BASE64Decoder().decodeBuffer(baStr), "UTF-8").split(":") match {
      case Array(user: String, pass: String) => (user, pass)
      case _                                 => ("", "")
    }
  }

  /**
   * Actions
   */
  def createToken() = Action.async {
    request =>
      {
        request.headers.get("Authorization") match {
          case None =>   Future.successful(resBadRequest("No Authorization field in header"))
          case Some(basicAuth) if !basicAuth.contains("Basic") =>
            Future.successful(resBadRequest("Missing keyword \"Basic\" in authorization header"))
          case Some(basicAuth) =>
            val (loginName, password) = decodeBasicAuth(basicAuth)
            val loginNameLower = loginName.toLowerCase
            //find account and get first identity
            Account.findByLoginName(loginNameLower).flatMap {
              case None => Future(resUnauthorized("Invalid password/loginName"))
              case Some(account) => if (account.identities.nonEmpty) {
                val identityId = account.identities(0)

                Identity.find(identityId).map {
                  case None => resNotFound("identity")
                  case Some(identity) =>
                    // check loginNames and passwords match
                    if (BCrypt.checkpw(password, account.password) && account.loginName.equals(loginNameLower)) {
                      // everything is ok
                      val token = Token.createDefault
                      identity.addToken(token)
                      resOk(token.toJson)
                    } else {
                      resUnauthorized("Invalid password/loginName")
                    }
                }
              } else {
                Future.successful(resUnauthorized("Invalid password/loginName"))
              }
            }
        }
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

}
