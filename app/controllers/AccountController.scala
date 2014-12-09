package controllers

import actors.{ ConfirmMail, ConfirmPhoneNumber }
import constants.ErrorCodes
import events.ContactUpdate
import helper.JsonHelper._
import helper.{ CheckHelper, JsonHelper }
import helper.ResultHelper._
import legacy.v1.AccountController._
import models._
import org.mindrot.jbcrypt.BCrypt
import play.api.Play.current
import play.api.i18n.Lang
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.mvc.{ Request, Action, Result }
import play.api.{ Logger, Play }
import play.modules.statsd.api.Statsd
import services.AuthenticationActions.AuthAction
import services.LocalizationMessages
import traits.ExtendedController
import play.api.libs.functional.syntax._
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

  case class CreateAccountRequest(loginName: String, password: String, reservationSecret: String)

  object CreateAccountRequest {
    implicit def reads: Reads[CreateAccountRequest] = (
      (__ \ 'loginName).read[String](toLowerCase) and
      (__ \ 'password).read[String](minLength[String](8) andKeep hashPassword) and
      (__ \ 'reservationSecret).read[String]
    )(CreateAccountRequest.apply _)
  }

  def createAccountNonAuth(request: Request[JsObject]): Future[Result] = {
    validateFuture(request.body, CreateAccountRequest.reads) {
      car =>
        AccountReservation.checkReservationSecret(car.loginName, car.reservationSecret).flatMap {
          case false => Future(resBadRequest("invalid reservation secret"))
          case true =>
            val account = Account.create(car.loginName, car.password)
            storeAccount(account, None)
        }
    }
  }

  def storeAccount(account: Account, identityId: Option[MongoId]): Future[Result] = {
    Account.insert(account).map {
      le =>
        // create statsd event when user is not a test user
        val testUserPrefix = Play.configuration.getString("testUser.prefix").getOrElse("foo")
        if (!account.loginName.startsWith(testUserPrefix.toLowerCase)) {
          Statsd.increment("custom.account.create")
        }
        identityId match {
          case None     => resOk(account.toJson)
          case Some(id) => resOk(account.toJsonWithIdentities(id))
        }
    }
  }

  def createAccount = AuthAction(allowExternal = true, nonAuthBlock = createAccountNonAuth).async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, CreateAccountRequest.reads) {
        car =>
          AccountReservation.checkReservationSecret(car.loginName, car.reservationSecret).flatMap {
            case false => Future(resBadRequest("invalid reservation secret"))
            case true =>
              val account = Account.create(car.loginName, car.password)

              // check if this identity is already registered
              request.identity.accountId match {
                case Some(i) => Future(resBadRequest("token belongs to a registered user"))
                case None =>

                  // find the user that added the external contact
                  val query = Json.obj("contacts.identityId" -> request.identity.id)
                  Identity.find(query).flatMap {
                    case None => Future(resBadRequest("this user is in nobodies contact book"))
                    case Some(otherIdentity) =>
                      val futureRes: Future[Boolean] = for {
                        // add other identity as contact
                        addContact <- request.identity.addContact(Contact.create(otherIdentity.id))
                        updateIdentity <- {
                          // update identity
                          val map = Map("cameoId" -> account.loginName,
                            "accountId" -> account.id,
                            "isDefaultIdentity" -> true)
                          Identity.update(request.identity.id, IdentityModelUpdate.fromMap(map))
                        }
                        deleteDetails <- {
                          val deleteValues = Seq("email", "phoneNumber", "displayName")
                          Identity.deleteValues(request.identity.id, deleteValues).map(_.updatedExisting)
                        }
                      } yield {
                        addContact && updateIdentity && deleteDetails
                      }
                      futureRes.flatMap {
                        case false => Future(resServerError("unable to update identity"))
                        case true =>
                          // send contact update event to other identity
                          Identity.find(request.identity.id).map {
                            case None => // do nothing
                            case Some(i) =>
                              otherIdentity.contacts.find(_.identityId.equals(request.identity.id)) match {
                                case None          => // do nothing
                                case Some(contact) => actors.eventRouter ! ContactUpdate(otherIdentity.id, contact, i)
                              }
                          }
                          storeAccount(account, Some(request.identity.id))
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
            case Some(account) => account.toJsonWithIdentities(request.identity.id).map(resOk[JsObject])
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

  def updateAccount() = AuthAction(getAccount = true).async(parse.tolerantJson) {
    request =>
      val lang = LocalizationMessages.getBrowserLanguage(request)

      request.identity.accountId match {
        case None => Future(resBadRequest("no account"))
        case Some(accountId) =>

          def doAccountUpdate(update: JsObject): Future[Result] = {
            Account.update(accountId, update).map {
              case false => resServerError("could not update")
              case true =>
                // check if update contains a phoneNumber or email. Start verification if it does
                if ((request.body \ "email").asOpt[String].isDefined) {
                  request.account.map {
                    account => actors.verificationRouter ! ConfirmMail(account.id, lang)
                  }
                }
                if ((request.body \ "phoneNumber").asOpt[String].isDefined) {
                  request.account.map {
                    account => actors.verificationRouter ! ConfirmPhoneNumber(account.id, lang)
                  }
                }

                resOk("updated")
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
