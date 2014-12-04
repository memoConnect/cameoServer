package controllers

import actors.{ VerifyMail, VerifyPhoneNumber, VerificationActor }
import akka.actor.Props
import constants.ErrorCodes
import constants.Verification._
import helper.ResultHelper._
import models._
import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{ Result, Action, Controller }
import services.{ LocalizationMessages, AuthenticationActions }
import services.AuthenticationActions.AuthAction
import traits.ExtendedController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object VerificationController extends Controller with ExtendedController {

  case class StartVerifyRequest(verifyPhoneNumber: Option[Boolean], verifyMail: Option[Boolean])
  object StartVerifyRequest { implicit val format = Json.format[StartVerifyRequest] }

  def startVerification() = AuthAction()(parse.tolerantJson) {
    request =>
      val lang = LocalizationMessages.getBrowserLanguage(request)
      request.identity.accountId match {
        case None => resBadRequest("identity has no account")
        case Some(accountId) =>
          validate[StartVerifyRequest](request.body, StartVerifyRequest.format) {
            svr =>
              if (svr.verifyPhoneNumber.getOrElse(false)) {
                actors.verificationRouter ! VerifyPhoneNumber(accountId, lang)
              }
              if (svr.verifyMail.getOrElse(false)) {
                actors.verificationRouter ! VerifyMail(accountId, lang)
              }
              resOk()
          }
      }
  }

  trait VerifyResult
  case object VerifySuccess extends VerifyResult
  case object VerifyExpired extends VerifyResult
  case object VerifyValueChanged extends VerifyResult
  case object VerifyError extends VerifyResult

  def verify(id: String, account: Option[Account]): Future[VerifyResult] = {
    VerificationSecret.find(new MongoId(id)).flatMap {
      case None => Future(VerifyExpired)
      case Some(verificationSecret) =>
        VerificationSecret.delete(verificationSecret.id)
        // check if accountId matches the one stored with the secret
        account match {
          case None =>
            Account.find(verificationSecret.accountId).map {
              case None    => VerifyError
              case Some(a) => applyVerification(a, verificationSecret)
            }
          case Some(a) if a.id.equals(verificationSecret.accountId) => Future(applyVerification(a, verificationSecret))
          case _                                                    => Future(VerifyError)
        }
    }
  }

  def applyVerification(account: Account, verificationSecret: VerificationSecret): VerifyResult = {
    verificationSecret.valueType match {
      case VERIFY_TYPE_MAIL =>
        account.email match {
          case None                                                                     => VerifyValueChanged
          case Some(email) if !email.value.equals(verificationSecret.valueToBeVerified) => VerifyValueChanged
          case Some(email) =>
            val set = Map("email" -> email.copy(isVerified = true))
            Account.update(account.id, AccountModelUpdate.fromMap(set))
            VerifySuccess
        }
      case VERIFY_TYPE_PHONENUMBER =>
        account.phoneNumber match {
          case None                                                                                 => VerifyValueChanged
          case Some(phoneNumber) if !phoneNumber.value.equals(verificationSecret.valueToBeVerified) => VerifyValueChanged
          case Some(phoneNumber) =>
            val set = Map("phoneNumber" -> phoneNumber.copy(isVerified = true))
            Account.update(account.id, AccountModelUpdate.fromMap(set))
            VerifySuccess
        }
      case _ => VerifySuccess
    }
  }

  def verifyLink(id: String) = Action.async {
    request =>
      val lang = LocalizationMessages.getBrowserLanguage(request)
      verify(id, None).map {
        case VerifySuccess      => Ok(views.html.verify(false, false, lang))
        case VerifyExpired      => Ok(views.html.verify(true, false, lang))
        case VerifyValueChanged => Ok(views.html.verify(false, true, lang))
        case VerifyError        => Ok(views.html.verify(false, true, lang))
      }
  }

  def verifyCode(id: String) = AuthAction(getAccount = true).async {
    request =>
      verify(id, Some(request.account.get)).map{
        case VerifySuccess      => resOk("verified")
        case VerifyExpired      => resBadRequest("", ErrorCodes.VERIFY_EXPIRED)
        case VerifyValueChanged => resBadRequest("", ErrorCodes.VERIFY_VALUE_CHANGED)
        case VerifyError        => resBadRequest("other error")
      }
  }
}