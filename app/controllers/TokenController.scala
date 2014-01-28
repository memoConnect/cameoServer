package controllers

import play.api.mvc._
import helper.IdHelper
import play.api.libs.json.{Json, JsValue}
import scala.concurrent.Future
import org.mindrot.jbcrypt.BCrypt
import traits.ExtendedController
import models.{MongoId, Identity, Account, Token}
import play.api.libs.concurrent.Execution.Implicits._
import helper.ResultHelper._


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
    val baStr = auth.replaceFirst("Basic ", "")
    val Array(user, pass) = new String(new sun.misc.BASE64Decoder().decodeBuffer(baStr), "UTF-8").split(":")
    (user, pass)
  }

  /**
   * Actions
   */
  def createToken(identityId: String) = Action.async {
    request => {
      request.headers.get("Authorization") match {
        case None => {
          Future.successful(BadRequest(resKO("No Authorization field in header")))
        }
        case Some(basicAuth) => {
          val (loginName, password) = decodeBasicAuth(basicAuth)
          val identityMongoId = new MongoId(identityId)

          Identity.find(identityMongoId).flatMap {
            case None => Future.successful(NotFound(resKO("Identity not found")))
            case Some(identity) => if (identity.accountId.isDefined) {
              Account.find(identity.accountId.get).map {
                account => {
                  // check loginNames and passwords match
                  if (BCrypt.checkpw(password, account.get.password) && account.get.loginName.equals(loginName)) {
                    // everything is ok
                    val token = Token.create(identityMongoId)
                    Token.col.insert(token)
                    resOK(token.toJson)
                  } else {
                    Unauthorized(resKO("Invalid password/loginName"))
                  }
                }
              }
            } else {
              Future.successful(Unauthorized(resKO("Invalid password/loginName")))
            }
          }
        }
      }
    }
  }

  def getTokenOptions = Action {
    request =>
      Ok("")
  }

  def deleteToken(token: String) = Action.async {
    request =>
      Token.col.remove[JsValue](Json.obj("_id" -> new MongoId(token))).map {
        lastError =>
          if (lastError.updated > 0) {
            resOK(Json.obj("deletedToken" -> token))
          }
          else if (lastError.ok) {
            NotFound(resKO("Token not found"))
          } else {
            InternalServerError(resKO(lastError.stringify))
          }
      }

  }

  def getToken(token: String) = Action.async {
    request =>
      Token.find(new MongoId(token)).map {
        case None => NotFound(resKO("token not found"))
        case Some(t) => resOK(t.toJson)
      }
  }
}
