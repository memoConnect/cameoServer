package controllers

import play.api.mvc._
import helper.IdHelper
import play.api.libs.json.{Json, JsValue}
import scala.concurrent.Future
import org.mindrot.jbcrypt.BCrypt
import traits.ExtendedController
import models.Token
import java.util.Date
import play.api.libs.concurrent.Execution.Implicits._
import services.Authentication


/**
 * User: Björn Reimer
 * Date: 5/28/13
 * Time: 4:30 PM
 */
object TokenController extends ExtendedController {

  /**
   * Helper
   */
  def checkUserAndReturnToken(user: String, pass: String): Future[SimpleResult] = {
    val futureUser: Future[Option[JsValue]] = userCollection.find(Json.obj("username" -> user)).one[JsValue]

    futureUser.map {
      case None => {
        Unauthorized(resKO("Wrong Username/Password"))
      }
      case Some(u: JsValue) => {
        // check password
        if (!BCrypt.checkpw(pass, (u \ "password").asOpt[String].getOrElse(""))) {
          // wrong password
          Unauthorized(resKO("Wrong Username/Password"))
        } else {
          val token = new Token(IdHelper.generateAccessToken(), Some(user), None, Some(Authentication.AUTH_USER), new Date)
          tokenCollection.insert(token).map {
            lastError => InternalServerError(resKO("MongoError: " + lastError))
          }
          Ok(resOK(Json.toJson(token)(Token.outputWrites))).withHeaders(
            ACCESS_CONTROL_ALLOW_HEADERS -> "Authorization")
        }
      }
    }
  }

  // decode username and password
  def decodeBasicAuth(auth: String) = {
    val baStr = auth.replaceFirst("Basic ", "")
    val Array(user, pass) = new String(new sun.misc.BASE64Decoder().decodeBuffer(baStr), "UTF-8").split(":")
    (user, pass)
  }

  // create new token
  def createToken(user: String): JsValue = {
    Json.obj("token" -> IdHelper.generateAccessToken(), "username" -> user)
  }

  /**
   * Actions
   */
  def getToken = Action.async {
    request => {
      request.headers.get("Authorization") match {
        case None => {
          BadRequest(resKO("No Authorization field in header")).withHeaders(
            WWW_AUTHENTICATE -> Authentication.AUTH_USER)
        }
        case Some(basicAuth) => {
          val (user, pass) = decodeBasicAuth(basicAuth)
          checkUserAndReturnToken(user, pass)
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
      tokenCollection.remove[JsValue](Json.obj("token" -> token)).map {
        lastError =>
          if (lastError.updated > 0) {
            Ok(resOK(Json.obj("deletedToken" -> token)))
          }
          else if (lastError.ok) {
            NotFound(resKO("Token not found"))
          } else {
            InternalServerError(resKO(lastError.stringify))
          }
      }

  }
}
