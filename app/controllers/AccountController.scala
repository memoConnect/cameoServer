package controllers

import play.api.mvc.{SimpleResult, Action}

import play.api.libs.json._
import traits.ExtendedController
import models.{Identity, MongoId, Account, User}
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

      jsBody.validate[Account](Account.createReads).map {
        account =>
          accountCollection.insert(account).map {
            lastError => {
              if (lastError.ok) {
                Ok(resOK(account.toJson))
              } else {
                InternalServerError(resKO("MongoError: " + lastError))
              }
            }
          }.recover {
            // deal with exceptions from duplicate loginNames
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

  def getAccount(loginName: String) = Action.async {
    request => Account.findByLoginName(loginName).map {
      case None => NotFound(resKO("Account not found: " + loginName))
      case Some(account) => Ok(resOK(account.toJson))
    }
  }

  def deleteAccount(loginName: String) = Action.async {
    request =>
      Account.col.remove[JsValue](Json.obj("loginName" -> loginName)).map {
        lastError =>
          if (lastError.updated > 0)
            Ok(resOK(Json.obj("deleted Account" -> loginName)))
          else if (lastError.ok) {
            NotFound(resKO("Account not found"))
          } else
            InternalServerError(resKO(lastError.stringify))
      }
  }
}
