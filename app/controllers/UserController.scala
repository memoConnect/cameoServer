package controllers

import play.api.mvc.{Result, Action}

import play.api.libs.json._
import traits.ExtendedController
import models.User
import reactivemongo.core.errors.DatabaseException


/**
 * User: Björn Reimer
 * Date: 5/16/13
 * Time: 10:58 AM
 */
object UserController extends ExtendedController {

  def createUser = Action(parse.tolerantJson) {
    request =>
      val jsBody: JsValue = request.body

      jsBody.validate[User](User.inputReads).map {
        user =>
          Async {
            userCollection.insert(Json.toJson(user)).map {
              lastError => {
                if (lastError.ok) {
                  Ok(resOK(Json.toJson(user)(User.outputWrites)))
                } else {
                  InternalServerError(resKO("MongoError: " + lastError))
                }
              }
            }.recover {
              // deal with exceptions from duplicate usernames or emails
              case de: DatabaseException =>
                if (de.getMessage().contains("username")) {
                  BadRequest(resKO("The username already exists"))
                } else if (de.getMessage().contains("email")) {
                  BadRequest(resKO("The email already exists"))
                } else {
                  BadRequest(resKO("Error: " + de.getMessage()))
                }
              case e => InternalServerError(resKO("Mongo Error: " + e.toString))

            }
          }
      }.recoverTotal(error => BadRequest(resKO(JsError.toFlatJson(error))))
  }

  def returnUser(username: String): Result = {
    Async {
      User.find(username).map {
        case Some(user: User) => Ok(resOK(Json.toJson(user)(User.outputWrites)))
        case None => NotFound(resKO("User not found: " + username))
      }
    }
  }

  def getUser(username: String) = Action {
    request => returnUser(username)
  }

  def getUserWithToken(token: String) = authenticateGET(token) {
    (username, request) => returnUser(username)
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
