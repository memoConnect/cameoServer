package controllers

import actors.VerificationActor
import akka.actor.Props
import constants.Verification._
import helper.ResultHelper._
import models._
import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{ Action, Controller }
import services.{ LocalizationMessages, AuthenticationActions }
import services.AuthenticationActions.AuthAction
import traits.ExtendedController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object VerificationController extends Controller with ExtendedController {
  def sendVerifyMessage() = AuthAction()(parse.tolerantJson) {
    request =>
      case class VerifyRequest(verifyPhoneNumber: Option[Boolean], verifyMail: Option[Boolean])

      val reads = (
        (__ \ "verifyPhoneNumber").readNullable[Boolean] and
        (__ \ "verifyEmail").readNullable[Boolean])(VerifyRequest.apply _)

      // TODO: Write tests for this
      validate[VerifyRequest](request.body, reads) {
        vr =>
          lazy val verifyActor = Akka.system.actorOf(Props[VerificationActor])

          if (vr.verifyPhoneNumber.getOrElse(false)) {
            verifyActor ! (VERIFY_TYPE_PHONENUMBER, request.identity)
          }
          if (vr.verifyMail.getOrElse(false)) {
            verifyActor ! (VERIFY_TYPE_MAIL, request.identity)
          }
          resOk()
      }
  }

  def verify(id: String) = Action.async {
    request =>
      val lang = LocalizationMessages.getBrowserLanguage(request)
      VerificationSecret.find(new MongoId(id)).flatMap {
        case None => Future(Ok(views.html.verify(true, false, lang)))
        case Some(verificationSecret) =>
          VerificationSecret.delete(verificationSecret.id)
          Account.find(verificationSecret.accountId).map {
            case None => Ok(views.html.verify(false, true, lang))
            case Some(account) => verificationSecret.valueType match {
              case VERIFY_TYPE_MAIL =>
                account.email match {
                  case None                                                                     => Ok(views.html.verify(false, true, lang))
                  case Some(email) if !email.value.equals(verificationSecret.valueToBeVerified) => Ok(views.html.verify(false, true, lang))
                  case Some(email) =>
                    val set = Map("email" -> email.copy(isVerified = true))
                    Account.update(account.id, AccountModelUpdate.fromMap(set))
                    Ok(views.html.verify(false, false, lang))
                }
              case VERIFY_TYPE_PHONENUMBER =>
                account.phoneNumber match {
                  case None                                                                                 => Ok(views.html.verify(false, true, lang))
                  case Some(phoneNumber) if !phoneNumber.value.equals(verificationSecret.valueToBeVerified) => Ok(views.html.verify(false, true, lang))
                  case Some(phoneNumber) =>
                    val set = Map("phoneNumber" -> phoneNumber.copy(isVerified = true))
                    Account.update(account.id, AccountModelUpdate.fromMap(set))
                    Ok(views.html.verify(false, false, lang))
                }
              case _ => Ok(views.html.verify(false, true, lang))
            }
          }
      }
  }
}