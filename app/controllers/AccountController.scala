package controllers

import play.api.mvc.Action
import play.api.libs.json._
import traits.ExtendedController
import models._
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Future
import helper.ResultHelper._
import helper.CmActions.AuthAction
import scala.Some

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.ws.WS
import services.AvatarGenerator

/**
 * User: Björn Reimer
 * Date: 5/16/13
 * Time: 10:58 AM
 */
object AccountController extends ExtendedController {

  def checkLogin(login: String): Boolean = {
    login.length >= 6 &&
      login.length < 21 &&
      login.matches("^\\w+$")

  }

  case class AdditionalValues(reservationSecret: String, displayName: Option[String])

  object AdditionalValues {
    val reads: Reads[AdditionalValues] = Json.reads[AdditionalValues]
  }

  def createAccount = Action.async(parse.tolerantJson) {
    request =>
      val jsBody: JsValue = request.body

      validateFuture[AdditionalValues](jsBody, AdditionalValues.reads) {
        additionalValues =>
          validateFuture[Account](jsBody, Account.createReads) {
            account =>
              {
                AccountReservation.checkReserved(account.loginName.toLowerCase).flatMap {
                  case None => Future(resBadRequest("this loginName is not reserved"))
                  case Some(secret) =>

                    secret.equals(additionalValues.reservationSecret) match {
                      case false => Future(resBadRequest("invalid reservation secret"))
                      case true =>

                        // everything is ok, we can create the account now
                        AccountReservation.deleteReserved(account.loginName.toLowerCase)

                        // create identity and add it to account
                        val identity = Identity.create(Some(account.id), account.loginName, account.email, account.phoneNumber, additionalValues.displayName)
                        Identity.col.insert(identity)
                        // generate default avatar
                        AvatarGenerator.generate(identity)

                        val account2 = account.copy(identities = Seq(identity.id), loginName = account.loginName.toLowerCase)

                        Account.col.insert(account2).flatMap {
                          lastError =>
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

  def getAccount(loginName: String) = AuthAction().async {
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
          val lowerLogin = vr.loginName.toLowerCase

          if (checkLogin(vr.loginName)) {
            // check if loginName exists or is a cameoId
            val loginExists: Future[Boolean] = for {
              account <- Account.findByLoginName(lowerLogin)
              identity <- Identity.findByCameoId(vr.loginName)
            } yield {
              account.isDefined || identity.isDefined
            }

            loginExists.flatMap {
              // it exists, find alternative
              case true => Account.findAlternative(vr.loginName).map {
                newLoginName => resKO(Json.obj("alternative" -> newLoginName))
              }
              // it does not exist, check if it is reserved
              case false => AccountReservation.checkReserved(lowerLogin).flatMap {
                // it is reserved, get alternative
                case Some(ra) => Account.findAlternative(vr.loginName).map {
                  newLoginName => resKO(Json.obj("alternative" -> newLoginName))
                }
                // not reserved, reserve it and return reservation Secret
                case None => {
                  AccountReservation.reserve(lowerLogin).map {
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

  def deleteAccount(loginName: String) = AuthAction().async {
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
