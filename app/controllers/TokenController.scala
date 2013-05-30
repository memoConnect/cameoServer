package controllers

import play.api.mvc._
import play.modules.reactivemongo.MongoController
import helper.{IdHelper, MongoHelper}
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.json.{Json, JsValue, JsString}
import scala.concurrent.Future
import org.mindrot.jbcrypt.BCrypt

/**
 * User: Björn Reimer
 * Date: 5/28/13
 * Time: 4:30 PM
 */
object TokenController extends MongoHelper {

  /**
   * Future Actor to get Token   TODO
   */

  def checkUserAndGetToken(user: String, pass: String): Future[Result] = {
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
        }
        else {
          val token = createToken(user).transform(addCreateDate).get
          tokenCollection.insert(token).map {
            lastError => InternalServerError(resKO("MongoError: " + lastError))
          }
          Ok(resOK(token.transform(fromCreated).get))
        }
      }
    }
  }

  /**
   * Helper
   */

  // decode username and password
  def decodeBasicAuth(auth: String) = {
    val baStr = auth.replaceFirst("Basic ", "")
    val Array(user, pass) = new String(new sun.misc.BASE64Decoder().decodeBuffer(baStr), "UTF-8").split(":")
    (user, pass)
  }

  // create new token
  def createToken(user: String): JsValue = {
    Json.obj(
      "token" -> IdHelper.generateAccessToken(),
      "username" -> user
    )
  }

  /**
   * Actions
   */

  def getToken = Action {
    request =>
      request.headers.get("Authorization") match {
        case None => BadRequest(resKO("No Authorization field in header"))
        case Some(basicAuth) => {
          val (user, pass) = decodeBasicAuth(basicAuth)
          Async {
            checkUserAndGetToken(user, pass)
          }
        }
      }
  }

  def deleteToken(token: String) = Action {
    request =>
      Async {
        tokenCollection.remove[JsValue](Json.obj("token" -> token)).map {
          lastError =>
            if (lastError.updated > 0)
              Ok(resOK(Json.obj("deletedToken" -> token)))
            else if (lastError.ok) {
              NotFound(resKO("Token not found"))}
            else
              InternalServerError(resKO(lastError.stringify))
        }
      }
  }
}
