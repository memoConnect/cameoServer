package controllers

import actors.{ ConfirmMail, ConfirmPhoneNumber }
import constants.ErrorCodes
import events.{ AccountUpdate, ContactUpdate }
import helper.JsonHelper._
import helper.{ CheckHelper, JsonHelper }
import helper.ResultHelper._
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
import services.AuthenticationActions._
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
      (__ \ 'loginName).read[String] and
      (__ \ 'password).read[String](minLength[String](8) andKeep hashPassword) and
      (__ \ 'reservationSecret).read[String]
    )(CreateAccountRequest.apply _)
  }

  def createAccountNonAuth[A](request: Request[A]): Future[Result] = {
    request.body match {
      case js: JsValue =>
        validateFuture(js, CreateAccountRequest.reads) {
          car =>
            AccountReservation.checkReservationSecret(car.loginName, car.reservationSecret).flatMap {
              case false => Future(resBadRequest("invalid reservation secret"))
              case true =>
                val account = Account.create(car.loginName, car.password)
                storeAccount(account, None)
            }
        }
      case _ => Future(resBadRequest("bad content type"))
    }
  }

  def createAccount = AuthAction(allowExternal = true, nonAuthBlock = createAccountNonAuth).async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, CreateAccountRequest.reads) {
        car =>
          AccountReservation.checkReservationSecret(car.loginName, car.reservationSecret).flatMap {
            case false => Future(resBadRequest("invalid reservation secret"))
            case true =>
              // check if this identity is already registered
              request.identity.accountId match {
                case Some(i) => Future(resBadRequest("token belongs to a registered user"))
                case None =>
                  val account = Account.create(car.loginName.toLowerCase, car.password)

                  // find the user that added the external contact and add him as contact (if contact still exists)
                  val query = Json.obj("contacts.identityId" -> request.identity.id)
                  Identity.find(query).flatMap {
                    case None => storeAccount(account, Some(request.identity.id))
                    case Some(otherIdentity) =>
                      for {
                      // add other identity as contact
                        addContact <- request.identity.addContact(Contact.create(otherIdentity.id))
                        updateIdentity <- {
                          // update identity
                          val map = Map(
                            "cameoId" -> car.loginName,
                            "accountId" -> account.id,
                            "isDefaultIdentity" -> true
                          )
                          Identity.update(request.identity.id, IdentityModelUpdate.fromMap(map))
                          request.identity.addSupport()
                        }
                        deleteDetails <- request.identity.deleteDetails(deleteDisplayName = true)
                        if updateIdentity
                        eventsSend <- {
                          // get updated identity todo: this can be done without another db request
                          Identity.find(request.identity.id).map {
                            case None => // do nothing
                            case Some(i) =>
                              otherIdentity.contacts.find(_.identityId.equals(request.identity.id)) match {
                                case None => // do nothing
                                case Some(contact) => actors.eventRouter ! ContactUpdate(otherIdentity.id, contact, i)
                              }
                          }
                        }
                        result <- storeAccount(account, Some(request.identity.id))
                      } yield {
                        result
                      }
                  }
              }

          }
      }
  }


  def storeAccount(account: Account, identityId: Option[MongoId]): Future[Result] = {
    Account.insert(account).flatMap {
      le =>
        // create statsd event when user is not a test user
        val testUserPrefix = Play.configuration.getString("testUser.prefix").getOrElse("foo")
        if (!account.loginName.startsWith(testUserPrefix.toLowerCase)) {
          Statsd.increment("custom.account.create")
        }
        identityId match {
          case None     => Future(resOk(account.toJson))
          case Some(id) => account.toJsonWithIdentities(id).map(resOk[JsObject])
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

  def doReserveLogin(body: JsValue, ownLogin: Option[String]): Future[Result] = {

    def checkLoginExists(value: String): Future[Boolean] = {
      for {
        account <- Account.findByLoginName(value)
        identity <- Identity.findByCameoId(value)
      } yield {
        (account.isDefined && !ownLogin.exists(_.equals(account.get.loginName))) || identity.isDefined
      }
    }

    def findAlternative(value: String, count: Int = 1): Future[String] = {
      val currentTry = value + "_" + count

      checkLoginExists(currentTry).flatMap {
        case true => findAlternative(value, count + 1) // recursive futures ftw!
        case false =>
          // check if it is reserved
          AccountReservation.findByLoginName(currentTry).flatMap {
            case Some(r) => findAlternative(value, count + 1)
            case None    => Future(currentTry)
          }
      }
    }

    def reserveOrAlternative(login: String, ownLogin: Option[String]): Future[Result] = {

      checkLogin(login) match {
        case false => Future(resBadRequest("invalid loginName/cameoId"))
        case true =>
          // check if loginName exists or is a cameoId
          checkLoginExists(login).flatMap {
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

    validateFuture[CheckLoginRequest](body, CheckLoginRequest.format) {
      checkLoginRequest =>
        Logger.info("Login check: " + checkLoginRequest)
        checkLoginRequest match {
          case CheckLoginRequest(None, None)                     => Future(resBadRequest("either cameoId or loginName required"))
          case CheckLoginRequest(Some(loginName), Some(cameoId)) => Future(resBadRequest("can only have either cameoId or loginName"))
          case CheckLoginRequest(None, Some(cameoId))            => reserveOrAlternative(cameoId, ownLogin)
          case CheckLoginRequest(Some(loginName), None)          => reserveOrAlternative(loginName, None)
        }
    }
  }

  def reserveLoginNonAuth(request: Request[Any]): Future[Result] = {
    request.body match {
      case js: JsValue => doReserveLogin(js, None)
      case _           => Future(resBadRequest("bad content type"))
    }
  }

  def reserveLogin() = BasicAuthAction(reserveLoginNonAuth).async(parse.tolerantJson) {
    request =>
      doReserveLogin(request.body, Some(request.account.loginName))
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

  def doAccountUpdate(body: JsValue, account: Account, lang: Lang): Future[Result] = {

    import helper.CheckHelper._

    def doUpdate(update: JsObject): Future[Result] = {
      Account.update(account.id, update).map {
        case false => resServerError("could not update")
        case true =>
          // check if update contains a phoneNumber or email. Start verification if it does
          if ((body \ "email").asOpt[String].isDefined) {
            actors.verificationRouter ! ConfirmMail(account.id, lang)
          }
          if ((body \ "phoneNumber").asOpt[String].isDefined) {
            actors.verificationRouter ! ConfirmPhoneNumber(account.id, lang)
          }

          // check if registration was marked complete, send event if it was
          if ((body \ "registrationIncomplete").asOpt[Boolean].exists(b => !b)) {
            Identity.findByAccountId(account.id).map {
              _.map {
                identity =>
                  actors.eventRouter ! AccountUpdate(identity.id, account.id, Json.obj("registrationIncomplete" -> false))
              }
            }
          }

          resOk("updated")
      }
    }

    // check if email and phonenumber are correct. Return error codes when they are not.
    ((body \ "email").asOpt[String].map(s => s.isEmpty || checkEmail(s)), (body \ "phoneNumber").asOpt[String].map(s => s.isEmpty || checkPhoneNumber(s))) match {
      case (Some(false), Some(false)) => Future(resBadRequest("invalid email and phonenumber", ErrorCodes.EMAIL_INVALID ++ ErrorCodes.PHONENUMBER_INVALID))
      case (Some(false), _)           => Future(resBadRequest("invalid email", ErrorCodes.EMAIL_INVALID))
      case (_, Some(false))           => Future(resBadRequest("invalid phonenumber", ErrorCodes.PHONENUMBER_INVALID))
      case _ =>

        AccountModelUpdate.fromRequest(body) {
          js =>
            // check if there is a password change
            val newPassword = (body \ "password").asOpt[String](JsonHelper.hashPassword)
            val oldPassword = (body \ "oldPassword").asOpt[String]

            (newPassword, oldPassword) match {
              case (None, _)           => doUpdate(js)
              case (Some(newPw), None) => Future(resBadRequest("old password required"))
              case (Some(newPw), Some(oldPw)) =>
                BCrypt.checkpw(oldPw, account.password) match {
                  case false => Future(resBadRequest("invalid old password"))
                  case true =>
                    val set = Map("password" -> newPw)
                    val update = js.deepMerge(AccountModelUpdate.fromMap(set))
                    doUpdate(update)
                }
            }
        }
    }
  }

  def updateAccount() = AuthAction(getAccount = true).async(parse.tolerantJson) {
    request =>
      val lang = LocalizationMessages.getBrowserLanguage(request)

      request.identity.accountId match {
        case None => Future(resBadRequest("no account"))
        case Some(accountId) =>
          Account.find(accountId).flatMap {
            case None          => Future(resServerError("Error getting account"))
            case Some(account) => doAccountUpdate(request.body, account, lang)
          }
      }
  }

  def updateInitialAccount() = BasicAuthAction().async(parse.tolerantJson) {
    request =>
      val lang = LocalizationMessages.getBrowserLanguage(request)
      doAccountUpdate(request.body, request.account, lang)
  }

}
