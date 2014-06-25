package controllers

import helper.CmActions.AuthAction
import helper.ResultHelper._
import models._
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.mvc.{Action, Result}
import traits.ExtendedController

import scala.concurrent.Future

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
                def createAccountWithIdentity(identity: Identity): Future[Result] = {
                  val accountWithIdentity = account.copy(identities = Seq(identity.id), loginName = account.loginName.toLowerCase)

                  // add support user
                  identity.addSupport()

                  Account.col.insert(accountWithIdentity).flatMap {
                    lastError =>
                      lastError.ok match {
                        case true =>
                          accountWithIdentity.toJsonWithIdentities.map(resOk)
                        case false =>
                          Future(resServerError("MongoError: " + lastError))
                      }
                  }
                }

                AccountReservation.checkReserved(account.loginName.toLowerCase).flatMap {
                  case None => Future(resBadRequest("this loginName is not reserved"))
                  case Some(secret) =>

                    secret.equals(additionalValues.reservationSecret) match {
                      case false => Future(resBadRequest("invalid reservation secret"))
                      case true =>
                        // delete reservation secret
                        AccountReservation.deleteReserved(account.loginName.toLowerCase)

                        // check if there is a token
                        request.headers.get("Authorization") match {
                          case None =>
                            // no token, create new identity
                            Identity.createAndInsert(Some(account.id), account.loginName, None, None, additionalValues.displayName)
                              .flatMap(createAccountWithIdentity)

                          case Some(token) =>
                            // there is a token, check if it belongs to an external user
                            Identity.findByToken(new MongoId(token)).flatMap {
                              case None => Future(resBadRequest("invalid token"))
                              case Some(identity) if identity.accountId.isDefined => Future(resBadRequest("token belongs to a registered user"))
                              case Some(identity) =>

                                // find the user that added the external contact
                                val query = Json.obj("contacts.identityId" -> identity.id)
                                Identity.find(query).flatMap {
                                  case None => Future(resBadRequest("this user is in nobodies contact book"))
                                  case Some(otherIdentity) =>
                                    // add other identity as contact
                                    identity.addContact(Contact.create(otherIdentity.id))

                                    // add new information to identity
                                    val update = IdentityUpdate(
                                      None,
                                      None,
                                      additionalValues.displayName,
                                      Some(account.loginName),
                                      Some(account.id)
                                    )
                                    identity.update(update).flatMap {
                                      case false => Future(resServerError("unable to update identity"))
                                      case true  => createAccountWithIdentity(identity)
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

  def getAccount(loginName: String) = AuthAction().async {
    request =>
      Account.findByLoginName(loginName).flatMap {
        case None          => Future(resNotFound("account"))
        case Some(account) => account.toJsonWithIdentities.map { resOk(_) }
      }
  }

  def checkLoginName = Action.async(parse.tolerantJson) {
    request =>
      case class VerifyRequest(loginName: String)

      val reads   = (__ \ 'loginName).read[String].map {
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
                case None =>
                  AccountReservation.reserve(lowerLogin).map {
                    res =>
                      resOk(res.toJson)
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
            resOk(Json.obj("deleted Account" -> loginName))
          } else if (lastError.ok) {
            resNotFound("account")
          } else {
            resServerError(lastError.stringify)
          }
      }
  }
}
