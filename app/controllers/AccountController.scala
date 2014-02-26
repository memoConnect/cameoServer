package controllers

import play.api.mvc.Action
import play.api.libs.json._
import traits.ExtendedController
import models._
import reactivemongo.core.errors.DatabaseException
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Future
import helper.ResultHelper._
import helper.JsonHelper._
import helper.{ IdHelper, UserNotification, AuthAction }
import play.api.libs.json
import scala.Some
import constants.Messaging._
import scala.Some
import scala.Some
import scala.Some
import java.util.Date

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._

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

  case class AdditionalValues(
    reservationSecret: String,
    cameoId: String)

  object AdditionalValues {
    val reads: Reads[AdditionalValues] = (
      (__ \ 'reservationSecret).read[String] and
      (__ \ 'cameoId).read[String]
    )(AdditionalValues.apply _)
  }

  def createAccount = Action.async(parse.tolerantJson) {
    request =>
      val jsBody: JsValue = request.body

      validateFuture[AdditionalValues](jsBody, AdditionalValues.reads) {
        additionalValues =>
          // check if cameoId exists
          Identity.findCameoId(additionalValues.cameoId).flatMap {
            case Some(i) => Future(resKO(errorNotify("cameo id already exists")))
            case None => {
              validateFuture[Account](jsBody, Account.createReads) {
                account =>
                  {

                    AccountReservation.checkReserved(account.loginName).flatMap {
                      case None => Future(resBadRequest("this loginName is not reserved"))
                      case Some(secret) => {

                        secret.equals(additionalValues.reservationSecret) match {
                          case false => Future(resBadRequest("invalid reservation secret"))
                          case true => {

                            // everything is ok, we can create the account now
                            AccountReservation.deleteReserved(account.loginName)

                            // create identity and add it to account
                            val identity = Identity.create(Some(account.id), additionalValues.cameoId, account.email, account.phoneNumber)
                            Identity.col.insert(identity)
                            val account2 = account.copy(identities = Seq(identity.id))

                            Account.col.insert(account2).flatMap {
                              lastError =>
                                {
                                  if (lastError.ok) {
                                    account2.toJsonWithIdentities.map { resOK(_) }
                                  } else {
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
          } else {
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
          } else if (lastError.ok) {
            resNotFound("account")
          } else {
            resServerError(lastError.stringify)
          }
      }
  }
}
