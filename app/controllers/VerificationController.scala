package controllers

import helper.AuthAction
import traits.ExtendedController
import play.api.mvc.{ Action, Controller }
import helper.ResultHelper._

import play.api.libs.json._
import play.api.libs.functional.syntax._
import constants.Verification._
import models.{ Identity, VerificationSecret, MongoId }
import scala.concurrent.{ ExecutionContext, Future }
import ExecutionContext.Implicits.global

object VerificationController extends Controller with ExtendedController {
  def sendVerifyMessage() = AuthAction(parse.tolerantJson) {
    request =>
      case class VerifyRequest(verifyPhoneNumber: Option[Boolean], verifyMail: Option[Boolean])

      val reads = (
        (__ \ "verifyPhoneNumber").readNullable[Boolean] and
        (__ \ "verifyEmail").readNullable[Boolean])(VerifyRequest.apply _)

      // TODO: Write tests for this
      request.body.validate[VerifyRequest](reads).map {
        vr =>
          if (vr.verifyPhoneNumber.getOrElse(false)) {
            actors.verifyActor ! (VERIFY_TYPE_PHONENUMBER, request.identity)
          }
          if (vr.verifyMail.getOrElse(false)) {
            actors.verifyActor ! (VERIFY_TYPE_MAIL, request.identity)
          }
          resOK()
      }.recoverTotal(e => BadRequest(resKO(JsError.toFlatJson(e))))
  }

  def verifyMessage(id: String) = Action.async {

    VerificationSecret.find(new MongoId(id)).flatMap {
      case None => Future(Unauthorized(resKO("invalid authorisation secret")))
      case Some(vs) => {
        // set verified boolean to true
        Identity.find(vs.identityId).map {
          case None => Unauthorized(resKO("identity not found"))
          case Some(i) => vs.verificationType match {
            case VERIFY_TYPE_MAIL => {
              if (i.email.map {
                _.toString
              }.getOrElse("").equalsIgnoreCase(vs.valueToBeVerified)) {
                val newMail = i.email.get.copy(isVerified = true)
                i.update(email = Some(newMail))
                resOK("verified")
              }
              else {
                Unauthorized(resKO("mail has changed"))
              }
            }
            case VERIFY_TYPE_PHONENUMBER => {
              if (i.phoneNumber.map {
                _.toString
              }.getOrElse("").equalsIgnoreCase(vs.valueToBeVerified)) {
                val newTel = i.phoneNumber.get.copy(isVerified = true)
                i.update(phoneNumber = Some(newTel))
                resOK("verified")
              }
              else {
                Unauthorized(resKO("phonenumber has changed"))
              }
            }
          }
        }

      }

    }

  }
}