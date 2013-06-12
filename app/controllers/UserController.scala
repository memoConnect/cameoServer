package controllers

import play.api.mvc.Action

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import org.mindrot.jbcrypt.BCrypt
import play.api.libs.json.JsString
import play.api.libs.json.JsObject
import helper.ExtendedController
import reactivemongo.api.indexes.{IndexType, Index}
import scala.concurrent.Future


/**
 * User: Björn Reimer
 * Date: 5/16/13
 * Time: 10:58 AM
 */
object UserController extends ExtendedController {

  /**
   * JSON Transfomer
   */
  // Validates a user
  val validateUser: Reads[JsObject] = ((__ \ 'username).json.pickBranch(Reads.of[JsString]) and
    (__ \ 'email).json.pickBranch(Reads.of[JsString] keepAnd Reads.email) and
    (__ \ 'password).json.pickBranch(Reads.of[JsString] keepAnd Reads.minLength[String](8)) and
    ((__ \ 'name).json.pickBranch(Reads.of[JsString]) or emptyObj) and
    ((__ \ 'phonenumber).json.pickBranch(Reads.of[JsString]) or emptyObj)).reduce

  // hash the password
  val hashPassword: Reads[JsObject] = {
    (__ \ 'password).json.update(of[JsString].map {
      case JsString(pass: String) => JsString(BCrypt.hashpw(pass, BCrypt.gensalt()))
    })
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
  def createUser = Action(parse.tolerantJson) {
    request =>
      val jsBody: JsValue = request.body

      userCollection.indexesManager.ensure(Index(List("username" -> IndexType.Ascending), unique = true, sparse = true))

      jsBody.transform(validateUser andThen hashPassword andThen addObjectIdAndDate).map {
        jsRes => Async {
          userCollection.insert(jsRes).map {
            lastError => {
              if (lastError.ok) {
                Ok(resOK(jsRes.transform((__ \ 'username).json.pickBranch).get))
              } else {
                InternalServerError(resKO("MongoError: " + lastError))
              }
            }
          }.recover {
            case e =>
              BadRequest(resKO("The username already exists"))
          }
        }
      }.recoverTotal(error => BadRequest(resKO(JsError.toFlatJson(error))))
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
          case None => NotFound(resKO("User not found: " + username))
        }
      }
  }

  def deleteUser(username: String) = Action {
    request =>
      Async {
        userCollection.remove[JsValue](Json.obj("username" -> username)).map {
          lastError =>
            if (lastError.updated > 0)
              Ok(resOK(Json.obj("deletedUser" -> username)))
            else if (lastError.ok) {
              NotFound(resKO("User not found"))
            } else
              InternalServerError(resKO(lastError.stringify))
        }
      }
  }

}
