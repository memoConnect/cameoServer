package controllers

import events.IdentityUpdate
import helper.JsonHelper
import helper.ResultHelper._
import models._
import org.mindrot.jbcrypt.BCrypt
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import play.api.mvc.{ Action, Result }
import play.api.{ Logger, Play }
import play.modules.statsd.api.Statsd
import services.AuthenticationActions.AuthAction
import services.AuthenticationActions
import traits.ExtendedController

import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 5/16/13
 * Time: 10:58 AM
 */
object AccountController extends ExtendedController {

  def checkLogin(login: String): Boolean = {
    login.length >= 3 &&
      login.length < 41 &&
      login.matches("^[\\w.]+$") &&
      login.count(_.isLetterOrDigit) > 0
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
          validateFuture[Account](jsBody, Account.createReads()) {
            account =>
              {
                def createAccountWithIdentity(identity: Identity): Future[Result] = {
                  val accountLowerCase = account.copy(loginName = account.loginName.toLowerCase)

                  // add support user
                  identity.addSupport()

                  Account.col.insert(accountLowerCase).flatMap {
                    lastError =>
                      lastError.ok match {
                        case true =>
                          // create statsd event when user is not a test user
                          val testUserPrefix = Play.configuration.getString("testUser.prefix").getOrElse("foo")
                          if (!accountLowerCase.loginName.startsWith(testUserPrefix.toLowerCase)) {
                            Statsd.increment("custom.account.create")
                          }
                          accountLowerCase.toJsonWithIdentities(identity.id).map(resOk)
                        case false =>
                          Future(resServerError("MongoError: " + lastError))
                      }
                  }
                }

                AccountReservation.checkReservationSecret(account.loginName, additionalValues.reservationSecret).flatMap {
                  case false => Future(resBadRequest("invalid reservation secret"))
                  case true =>

                    // check if there is a token
                    request.headers.get("Authorization") match {
                      case None =>
                        // no token, create new identity
                        Identity.createAndInsert(Some(account.id), account.loginName, None, None, true, additionalValues.displayName)
                          .flatMap(createAccountWithIdentity)

                      case Some(token) =>
                        // there is a token, check if it belongs to an external user
                        Identity.findByToken(new MongoId(token)).flatMap {
                          case None                                           => Future(resBadRequest("invalid token"))
                          case Some(identity) if identity.accountId.isDefined => Future(resBadRequest("token belongs to a registered user"))
                          case Some(identity) =>

                            // find the user that added the external contact
                            val query = Json.obj("contacts.identityId" -> identity.id)
                            Identity.find(query).flatMap {
                              case None => Future(resBadRequest("this user is in nobodies contact book"))
                              case Some(otherIdentity) =>
                                val futureRes: Future[Boolean] = for {
                                  // add other identity as contact
                                  addContact <- identity.addContact(Contact.create(otherIdentity.id))
                                  updateIdentity <- {
                                    // add update identity with details from registration
                                    val set = Map("cameoId" -> account.loginName,
                                      "accountId" -> account.id,
                                      "isDefaultIdentity" -> true,
                                      "displayName" -> additionalValues.displayName.getOrElse(""))
                                    Identity.update(identity.id, IdentityModelUpdate.fromMap(set))
                                  }
                                  deleteDetails <- {
                                    val deleteValues =
                                      Seq("email", "phoneNumber") ++ {
                                        additionalValues.displayName match {
                                          case None    => Seq("displayName")
                                          case Some(d) => Seq()
                                        }
                                      }
                                    Identity.deleteValues(identity.id, deleteValues).map(_.updatedExisting)
                                  }
                                } yield {
                                  addContact && updateIdentity && deleteDetails
                                }
                                futureRes.flatMap {
                                  case false => Future(resServerError("unable to update identity"))
                                  case true =>
                                    // send identity update event to other identity
                                    Identity.find(identity.id).map {
                                      case None    => // do nothing
                                      case Some(i) => actors.eventRouter ! IdentityUpdate(otherIdentity.id, identity.id, i.toPublicJson())
                                    }
                                    createAccountWithIdentity(identity)
                                }
                            }
                        }
                    }
                }

              }
          }
      }
  }

  def getAccount = AuthAction().async {
    request =>
      request.identity.accountId match {
        case None => Future(resBadRequest("no account"))
        case Some(id) =>
          Account.find(id).flatMap {
            case None          => Future(resNotFound("account"))
            case Some(account) => account.toJsonWithIdentities(request.identity.id).map(resOk)
          }
      }
  }

  case class CheckLoginRequest(loginName: Option[String], cameoId: Option[String])
  object CheckLoginRequest { implicit val format = Json.format[CheckLoginRequest] }

  def checkLoginName = Action.async(parse.tolerantJson) {
    request =>

      def findAlternative(value: String, count: Int = 1): Future[String] = {
        val currentTry = value + "_" + count

        val loginExists: Future[Boolean] = for {
          account <- Account.findByLoginName(currentTry)
          identity <- Identity.findByCameoId(currentTry)
        } yield {
          account.isDefined || identity.isDefined
        }

        loginExists.flatMap {
          case true => findAlternative(value, count + 1) // recursive futures ftw!
          case false =>
            // check if it is reserved
            AccountReservation.findByLoginName(currentTry).flatMap {
              case Some(r) => findAlternative(value, count + 1)
              case None    => Future(currentTry)
            }
        }
      }

      def reserveOrAlternative(login: String): Future[Result] = {

        checkLogin(login) match {
          case false => Future(resBadRequest("invalid loginName/cameoId"))
          case true =>
            // check if loginName exists or is a cameoId
            val maybeExists: Future[Boolean] = for {
              account <- Account.findByLoginName(login)
              identity <- Identity.findByCameoId(login)
            } yield {
              account.isDefined || identity.isDefined
            }

            maybeExists.flatMap {
              case true =>
                // it exists, find alternative
                findAlternative(login).map {
                  newLoginName => resKo(Json.obj("alternative" -> newLoginName))
                }
              case false =>
                // it does not exist, check if it is reserved
                AccountReservation.findByLoginName(login).flatMap {
                  // it is reserved, get alternative
                  case Some(ra) =>
                    findAlternative(login).map {
                      newLoginName => resKo(Json.obj("alternative" -> newLoginName))
                    }
                  // not reserved, reserve it and return reservation Secret
                  case None =>
                    AccountReservation.reserve(login).map {
                      res =>
                        resOk(res.toJson)
                    }
                }
            }
        }
      }

      validateFuture[CheckLoginRequest](request.body, CheckLoginRequest.format) {
        checkLoginRequest =>
          Logger.info("Login check: " + checkLoginRequest)
          checkLoginRequest match {
            case CheckLoginRequest(None, None)                     => Future(resBadRequest("either cameoId or loginName required"))
            case CheckLoginRequest(Some(loginName), Some(cameoId)) => Future(resBadRequest("can only have either cameoId or loginName required"))
            case CheckLoginRequest(None, Some(cameoId))            => reserveOrAlternative(cameoId)
            case CheckLoginRequest(Some(loginName), None)          => reserveOrAlternative(loginName)
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

  def updateAccount() = AuthAction().async(parse.tolerantJson) {
    request =>
      request.identity.accountId match {
        case None => Future(resBadRequest("no account"))
        case Some(accountId) =>

          def doAccountUpdate(update: JsObject): Future[Result] = {
            Account.update(accountId, update).map {
              case false => resServerError("could not update")
              case true  => resOk("updated")
            }
          }

          AccountModelUpdate.fromRequest(request.body) {
            js =>
              // check if there is a password change
              val newPassword = (request.body \ "password").asOpt[String](JsonHelper.hashPassword)
              val oldPassword = (request.body \ "oldPassword").asOpt[String]

              (newPassword, oldPassword) match {
                case (None, _)           => doAccountUpdate(js)
                case (Some(newPw), None) => Future(resBadRequest("old password required"))
                case (Some(newPw), Some(oldPw)) =>
                  Account.find(accountId).flatMap {
                    case None => Future(resServerError("could not retreive account"))
                    case Some(account) =>
                      BCrypt.checkpw(oldPw, account.password) match {
                        case false => Future(resBadRequest("invalid old password"))
                        case true =>
                          val set = Map("password" -> newPw)
                          val update = js.deepMerge(AccountModelUpdate.fromMap(set))
                          doAccountUpdate(update)
                      }
                  }
              }
          }
      }
  }

}
