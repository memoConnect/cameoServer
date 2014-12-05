package controllers

import actors.{ ConfirmMail, ConfirmPhoneNumber }
import constants.ErrorCodes
import constants.Confirmation._
import controllers.AccountController._
import events.AccountUpdate
import helper.CheckHelper
import helper.ResultHelper._
import models._
import play.api.i18n.Lang
import play.api.libs.json._
import play.api.mvc.{ Result, Action, Controller }
import services.AuthenticationActions.AuthAction
import services.LocalizationMessages
import traits.ExtendedController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ConfirmationController extends Controller with ExtendedController {

  case class ManualVerifyRequest(verifyPhoneNumber: Option[Boolean], verifyEmail: Option[Boolean])
  object ManualVerifyRequest { implicit val format = Json.format[ManualVerifyRequest] }

  def manualVerification() = AuthAction()(parse.tolerantJson) {
    request =>
      val lang = LocalizationMessages.getBrowserLanguage(request)
      request.identity.accountId match {
        case None => resBadRequest("identity has no account")
        case Some(accountId) =>
          validate[ManualVerifyRequest](request.body, ManualVerifyRequest.format) {
            svr =>
              if (svr.verifyPhoneNumber.getOrElse(false)) {
                actors.verificationRouter ! ConfirmPhoneNumber(accountId, lang)
              }
              if (svr.verifyEmail.getOrElse(false)) {
                actors.verificationRouter ! ConfirmMail(accountId, lang)
              }
              resOk()
          }
      }
  }

  def verifyLink(id: String) = Action.async {
    request =>
      val lang = LocalizationMessages.getBrowserLanguage(request)

      confirm(Right(new MongoId(id)), None, CONFIRMATION_TYPE_VERIFICATION, applyVerification).map {
        case ConfirmSuccess      => Ok(views.html.confirm(false, "BACKEND.VERIFICATION.LANDING_PAGE.SUCCESS", lang))
        case ConfirmExpired      => Ok(views.html.confirm(true, "BACKEND.VERIFICATION.LANDING_PAGE.EXPIRED", lang))
        case ConfirmValueChanged => Ok(views.html.confirm(true, "BACKEND.VERIFICATION.LANDING_PAGE.ERROR", lang))
        case ConfirmError        => Ok(views.html.confirm(true, "BACKEND.VERIFICATION.LANDING_PAGE.ERROR", lang))
      }
  }

  def verifyCode(id: String) = AuthAction(getAccount = true).async {
    request =>
      confirm(Left(id), Some(request.account.get), CONFIRMATION_TYPE_VERIFICATION, applyVerification).map {
        case ConfirmSuccess      => resOk("verified")
        case ConfirmExpired      => resBadRequest("", ErrorCodes.VERIFY_EXPIRED)
        case ConfirmValueChanged => resBadRequest("", ErrorCodes.VERIFY_VALUE_CHANGED)
        case ConfirmError        => resBadRequest("other error")
      }
  }

  def applyVerification(account: Account, confirmationSecret: ConfirmationToken): ConfirmResult = {
    confirmationSecret.confirmationPath match {
      case CONFIRMATION_PATH_MAIL =>
        account.email match {
          case None                                                                       => ConfirmValueChanged
          case Some(email) if !email.value.equals(confirmationSecret.confirmationAddress) => ConfirmValueChanged
          case Some(email) =>
            val map = Map("email" -> email.copy(isVerified = true))
            Account.update(account.id, AccountModelUpdate.fromMap(map))
            sendAccountUpdateEvent(account, Json.toJson(map).as[JsObject])
            ConfirmSuccess
        }
      case CONFIRMATION_PATH_PHONENUMBER =>
        account.phoneNumber match {
          case None                                                                                   => ConfirmValueChanged
          case Some(phoneNumber) if !phoneNumber.value.equals(confirmationSecret.confirmationAddress) => ConfirmValueChanged
          case Some(phoneNumber) =>
            val map = Map("phoneNumber" -> phoneNumber.copy(isVerified = true))
            Account.update(account.id, AccountModelUpdate.fromMap(map))
            sendAccountUpdateEvent(account, Json.toJson(map).as[JsObject])
            ConfirmSuccess
        }
      case _ => ConfirmSuccess
    }
  }

  def sendAccountUpdateEvent(account: Account, updatedValues: JsObject) = {
    Identity.findAll(Json.obj("accountId" -> account.id)).map {
      _.foreach {
        identity => actors.eventRouter ! AccountUpdate(identity.id, account.id, updatedValues)
      }
    }
  }

  case class StartResetPasswordRequest(identifier: String, foo: Option[Boolean])
  object StartResetPasswordRequest { implicit val format = Json.format[StartResetPasswordRequest] }

  def startPasswordReset() = Action.async(parse.tolerantJson) {

    def resetWithPhoneNumber(phoneNumber: String, lang: Lang): Future[Result] = {
      val query = Json.obj(
        "phoneNumber" -> Json.obj(
          "value" -> phoneNumber,
          "isVerified" -> true
        ))

      Account.findAll(query).map {
        case Seq()    => resBadRequest("", ErrorCodes.PASSWORD_RESET_PHONENUMBER_NOT_FOUND)
        case accounts => resetWithAccounts(accounts, lang)
      }
    }

    def resetWithEmail(email: String, lang: Lang): Future[Result] = {
      val query = Json.obj(
        "email" -> Json.obj(
          "value" -> email,
          "isVerified" -> true
        ))

      Account.findAll(query).map {
        case Seq()    => resBadRequest("", ErrorCodes.PASSWORD_RESET_EMAIL_NOT_FOUND)
        case accounts => resetWithAccounts(accounts, lang)
      }
    }

    def resetWithLoginOrCameoId(identifier: String, lang: Lang): Future[Result] = {
      // start with search for loginName
      Account.findByLoginName(identifier).flatMap {
        case Some(account) => Future(resetWithAccounts(Seq(account), lang))
        case None =>
          // search for cameoId
          Identity.findByCameoId(identifier).flatMap {
            case None => Future(resBadRequest("", ErrorCodes.PASSWORD_RESET_LOGIN_NOT_FOUND))
            case Some(identity) =>
              identity.accountId match {
                case None => Future(resBadRequest("", ErrorCodes.PASSWORD_RESET_LOGIN_NOT_FOUND))
                case Some(accountId) =>
                  Account.find(accountId).map {
                    case None          => resBadRequest("", ErrorCodes.PASSWORD_RESET_LOGIN_NOT_FOUND)
                    case Some(account) => resetWithAccounts(Seq(account), lang)
                  }
              }
          }
      }
    }

    def resetWithAccounts(accounts: Seq[Account], lang: Lang): Result = {
      accounts.map {
        account =>
          account.email match {
            case None        => // do nothing
            case Some(email) => actors.resetPasswordRouter ! ConfirmMail(account.id, lang)
          }
          account.phoneNumber match {
            case None              => // do nothing
            case Some(phoneNumber) => actors.resetPasswordRouter ! ConfirmPhoneNumber(account.id, lang)
          }
      }
      resOk("reset started")
    }

    request =>
      val lang = LocalizationMessages.getBrowserLanguage(request)
      validateFuture[StartResetPasswordRequest](request.body, StartResetPasswordRequest.format) {
        srpr =>
          // check what kind of value the identifier is
          CheckHelper.checkAndCleanMixed(srpr.identifier) match {
            case Some(Left(phoneNumber)) => resetWithPhoneNumber(phoneNumber, lang)
            case Some(Right(email))      => resetWithEmail(email, lang)
            case None                    => resetWithLoginOrCameoId(srpr.identifier, lang)
          }
      }
  }

  case class ResetPasswordRequest(newPassword: String, foo: Option[Boolean])
  object ResetPasswordRequest { implicit val format = Json.format[ResetPasswordRequest] }

  def resetPassword(id: String) = Action.async(parse.tolerantJson) {
    request =>
      validateFuture[ResetPasswordRequest](request.body, ResetPasswordRequest.format) {
        rpr =>
          // determine whether we are dealing with a code or id todo: find a more robust way to do this
          val idl = if (id.length < 10) Left(id) else Right(MongoId(id))
          confirm(idl, None, CONFIRMATION_TYPE_RESET_PASSWORD, applyPasswordReset(rpr)).map {
            case ConfirmSuccess => resOk("updated")
            case _              => resBadRequest("", ErrorCodes.PASSWORD_RESET_EXPIRED)
          }
      }
  }

  def applyPasswordReset(rpr: ResetPasswordRequest)(account: Account, confirmationSecret: ConfirmationToken): ConfirmResult = {
    val map = Map("password" -> rpr.newPassword)
    val update = AccountModelUpdate.fromMap(map)
    Account.update(account.id, update)
    ConfirmSuccess
  }

  def confirm(id: Either[String, MongoId], account: Option[Account], confirmationType: String, confirmedAction: (Account, ConfirmationToken) => ConfirmResult): Future[ConfirmResult] = {
    def deleteAndAction(a: Account, c: ConfirmationToken) = {
      ConfirmationToken.delete(c.id)
      confirmedAction(a, c)
    }
    id.fold(ConfirmationToken.findByCode, ConfirmationToken.find).flatMap {
      case None                                                                                    => Future(ConfirmExpired)
      case Some(confirmationToken) if !confirmationToken.confirmationType.equals(confirmationType) => Future(ConfirmError)
      case Some(confirmationToken) =>
        // check if accountIds match
        account match {
          case Some(a) if a.id.equals(confirmationToken.accountId) => Future(deleteAndAction(a, confirmationToken))
          case None if id.isRight =>
            Account.find(confirmationToken.accountId).map {
              case Some(a) => deleteAndAction(a, confirmationToken)
              case _       => ConfirmError
            }
          case _ => Future(ConfirmError)
        }
    }
  }

}