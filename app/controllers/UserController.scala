package controllers

import play.modules.reactivemongo.MongoController
import play.api.mvc.{Action, Controller}

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import org.mindrot.jbcrypt.BCrypt
import scala.concurrent.Future
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.json.JsString
import scala.Some
import play.api.libs.json.JsObject
import helper.MongoHelper
import reactivemongo.bson.BSONObjectID


/**
 * User: Björn Reimer
 * Date: 5/16/13
 * Time: 10:58 AM
 */


object UserController extends Controller with MongoController with MongoHelper{

  val userCollection: JSONCollection = db.collection[JSONCollection]("users")

  /**
   * JSON Transfomer
   */

  // Validates a user
  val validateUser: Reads[JsObject] = (

    (__ \ 'username).json.pickBranch(Reads.of[JsString]) and
      (__ \ 'email).json.pickBranch(Reads.of[JsString] keepAnd Reads.email) and
      (__ \ 'password).json.pickBranch(Reads.of[JsString] keepAnd Reads.minLength[String](8)) and
      ((__ \ 'name).json.pickBranch(Reads.of[JsString]) or emptyObj) and
      ((__ \ 'phonenumber).json.pickBranch(Reads.of[JsString]) or emptyObj)
    ).reduce

  // hash the password
  val hashPassword: Reads[JsObject] = {
    (__ \ 'password).json.update(of[JsString].map {
      case JsString(pass: String) => JsString(BCrypt.hashpw(pass, BCrypt.gensalt()))
    }
    )
  }

  // creates the output format for the user
  val outputUser: Reads[JsObject] = {
    fromCreated andThen
      (__ \ '_id).json.prune andThen
      (__ \ 'password).json.prune
  }



  /**
   * Actions
   */

  def createUser = Action(parse.json) {
    request =>
      val jsBody: JsValue = request.body

      jsBody.transform(validateUser andThen hashPassword andThen addObjectIdAndDate).map {
        jsRes => Async {
          userCollection.insert(jsRes).map {
            lastError => InternalServerError(resKO(JsString("MongoError: " + lastError)))
          }
        }
          Ok(resOK(jsRes.transform((__ \ 'username).json.pick).get))
      }.recoverTotal(
        error => BadRequest(resKO(JsError.toFlatJson(error)))
      )
  }

  def findUser(username: String) = Action {
    request =>
      Async {
        val futureUser: Future[Option[JsValue]] = userCollection.find(Json.obj("username" -> username)).one[JsValue]
        futureUser.map {
          case Some(u: JsValue) => u.transform(outputUser).map {
            jsRes => Ok(resOK(jsRes))
          }.recoverTotal {
            error => BadRequest(resKO(JsError.toFlatJson(error)))
          }
          case None => NotFound(resKO(JsString("User not found: " + username)))
        }
      }
  }

  def deleteUser(username: String) = Action {
    request =>
      Async {
        userCollection.remove[JsValue](Json.obj("username" -> username)).map {
          lastError =>
            if (lastError.ok)
              Ok(resOK(JsString("User deleted: " + username)))
            else
              InternalServerError(resKO(JsString(lastError.stringify)))
        }
      }
  }
}
