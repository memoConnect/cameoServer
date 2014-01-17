package controllers

import play.api.mvc.{SimpleResult, Action}

import play.api.libs.json._
import traits.ExtendedController
import models.{Account, User}
import reactivemongo.core.errors.DatabaseException
import play.api.libs.concurrent.Execution.Implicits._
import helper.AuthAction
import scala.concurrent.Future
import play.api.Logger


/**
 * User: Björn Reimer
 * Date: 5/16/13
 * Time: 10:58 AM
 */
object AccountController extends ExtendedController {

  def createAccount = Action.async(parse.tolerantJson) {
    request =>
      val jsBody: JsValue = request.body

      jsBody.validate[Account](Account.inputReads).map {
        account =>
          //create new Identity
          accountCollection.insert(account).map {
            lastError => {
              if (lastError.ok) {
                Ok(resOK(account.toJson))
              } else {
                InternalServerError(resKO("MongoError: " + lastError))
              }
            }
          }.recover {
            // deal with exceptions from duplicate usernames or emails
            case de: DatabaseException =>
              if (de.getMessage().contains("loginName")) {
                BadRequest(resKO("The username already exists"))
              } else {
                BadRequest(resKO("Error: " + de.getMessage()))
              }
            case e => InternalServerError(resKO("Mongo Error: " + e.toString))
          }
      }.recoverTotal(error => Future.successful(BadRequest(resKO(JsError.toFlatJson(error)))))
  }

  def getAccount

//  def returnUser(username: String): Future[SimpleResult] = {
//    User.find(username).map {
//      case Some(user: User) => Ok(resOK(Json.toJson(user)(User.outputWrites)))
//      case None => NotFound(resKO("User not found: " + username))
//    }
//  }
//
//  def getUser(username: String) = Action.async {
//    request => returnUser(username)
//  }
//
//  def getUserWithToken(token: String) = AuthAction.async {
//    (request) =>
//      val tokenObject = request.token
//      returnUser(tokenObject.username.get)
//  }
//
//  def deleteUser(username: String) = Action.async {
//    request =>
//      userCollection.remove[JsValue](Json.obj("username" -> username)).map {
//        lastError =>
//          if (lastError.updated > 0)
//            Ok(resOK(Json.obj("deletedUser" -> username)))
//          else if (lastError.ok) {
//            NotFound(resKO("User not found"))
//          } else
//            InternalServerError(resKO(lastError.stringify))
//      }
//  }
}
