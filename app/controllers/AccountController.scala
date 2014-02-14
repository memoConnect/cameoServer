package controllers

import play.api.mvc.Action
import play.api.libs.json._
import traits.ExtendedController
import models.{ AccountReservation, Identity, Account }
import reactivemongo.core.errors.DatabaseException
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Future
import helper.ResultHelper._
import helper.MongoHelper._
import helper.AuthAction

/**
 * User: Björn Reimer
 * Date: 5/16/13
 * Time: 10:58 AM
 */
object AccountController extends ExtendedController {

  def checkLogin(login: String): Boolean = {
    login.length >= 6 &&
      login.length < 20 &&
      login.matches("^\\w+$")
  }

  def createAccount = Action.async(parse.tolerantJson) {
    request =>
      val jsBody: JsValue = request.body
      val reservationSecret: Option[String] = (jsBody \ "reservationSecret").asOpt[String]

      validateFuture[Account](jsBody, Account.createReads) {

        account =>
          // check for reservation secret
          reservationSecret match {
            case None => Future(resBadRequest("no reservation secret"))
            case Some(rs) => AccountReservation.checkReserved(account.loginName).flatMap {

              case None => Future(resBadRequest("this loginName is not reserved"))
              case Some(secret) =>

                secret.equals(rs) match {
                  case false => Future(resBadRequest("invalid reservation secret"))
                  case true => {

                    // everything is ok, we can create the account now
                    AccountReservation.deleteReserved(account.loginName)

                    // create identity and add it to account
                    val identity = Identity.create(Some(account.id), account.email, account.phoneNumber)
                    Identity.col.insert(identity)
                    val account2 = account.copy(identities = Seq(identity.id))

                    accountCollection.insert(account2).flatMap {
                      lastError =>
                        {
                          if (lastError.ok) {
                            account2.toJsonWithIdentities.map { resOK(_) }
                          }
                          else {
                            Future(resServerError("MongoError: " + lastError))
                          }
                        }
                    }
                  }
                }
            }
          }
      }
  }

  def getAccount(loginName: String) = Action.async {
    request =>
      Account.findByLoginName(loginName).flatMap {
        case None          => Future(resNotFound("account"))
        case Some(account) => account.toJsonWithIdentities.map { resOK(_) }
      }
  }

  def checkLoginName = Action.async(parse.tolerantJson) {
    request =>
      case class VerifyRequest(loginName: String)

      val reads = (__ \ 'loginName).read[String].map {
        l => VerifyRequest(l)
      }

      validateFuture[VerifyRequest](request.body, reads) {
        vr =>
          if (checkLogin(vr.loginName)) {
            // check if loginName exists
            Account.findByLoginName(vr.loginName).flatMap {
              // it exists, find alternative
              case Some(a) => Account.findAlternative(vr.loginName).map {
                newLoginName => resKO(Json.obj("alternative" -> newLoginName))
              }
              // it does not exist, check if it is reserved
              case None => AccountReservation.checkReserved(vr.loginName).flatMap {
                // it is reserved, get alternative
                case Some(ra) => Account.findAlternative(vr.loginName).map {
                  newLoginName => resKO(Json.obj("alternative" -> newLoginName))
                }
                // not reserved, reserve it and return reservation Secret
                case None => {
                  AccountReservation.reserve(vr.loginName).map {
                    res =>
                      resOK(res.toJson)
                  }
                }
              }
            }
          }
          else {
            Future(resBadRequest("invalid login name"))
          }
      }
  }

  def deleteAccount(loginName: String) = AuthAction.async {
    request =>
      Account.col.remove[JsValue](Json.obj("loginName" -> loginName)).map {
        lastError =>
          if (lastError.updated > 0) {
            resOK(Json.obj("deleted Account" -> loginName))
          }
          else if (lastError.ok) {
            resNotFound("account")
          }
          else {
            resServerError(lastError.stringify)
          }
      }
  }
}
