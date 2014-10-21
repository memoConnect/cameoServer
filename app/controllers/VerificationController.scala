package controllers

import actors.VerifyActor
import akka.actor.Props
import constants.Verification._
import services.AuthenticationActions
import AuthenticationActions.AuthAction
import helper.ResultHelper._
import models.{ Identity, IdentityUpdate, MongoId, VerificationSecret }
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{ Action, Controller }
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

          lazy val verifyActor = Akka.system.actorOf(Props[VerifyActor])

          if (vr.verifyPhoneNumber.getOrElse(false)) {
            verifyActor ! (VERIFY_TYPE_PHONENUMBER, request.identity)
          }
          if (vr.verifyMail.getOrElse(false)) {
            verifyActor ! (VERIFY_TYPE_MAIL, request.identity)
          }
          resOk()
      }
  }

  def verifyMessage(id: String) = Action.async {

    VerificationSecret.find(new MongoId(id)).flatMap {
      case None => Future(resNotFound("verification secret"))
      case Some(vs) => {
        // set verified boolean to true
        Identity.find(vs.identityId).map {
          case None => resUnauthorized("identity not found")
          case Some(i) => vs.verificationType match {
            case VERIFY_TYPE_MAIL => {
              if (i.email.map {
                _.toString
              }.getOrElse("").equalsIgnoreCase(vs.valueToBeVerified)) {
                val set = Map("email" -> i.email.get.copy(isVerified = true))
                Identity.update(i.id, IdentityUpdate.setValues(set))
                resOk("verified")
              } else {
                resUnauthorized("mail has changed")
              }
            }
            case VERIFY_TYPE_PHONENUMBER => {
              if (i.phoneNumber.map {
                _.toString
              }.getOrElse("").equalsIgnoreCase(vs.valueToBeVerified)) {
                val set = Map("phoneNumber" -> i.email.get.copy(isVerified = true))
                Identity.update(i.id, IdentityUpdate.setValues(set))
                resOk("verified")
              } else {
                resUnauthorized("phonenumber has changed")
              }
            }
          }
        }
      }
    }
  }
}