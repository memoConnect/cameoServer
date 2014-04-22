package controllers


import helper.AuthRequestHelper.authAction
import traits.ExtendedController
import play.api.mvc.{ Action, Controller }
import helper.ResultHelper._

import play.api.libs.json._
import play.api.libs.functional.syntax._
import constants.Verification._
import models.{ IdentityUpdate, Identity, VerificationSecret, MongoId }
import scala.concurrent.{ ExecutionContext, Future }
import ExecutionContext.Implicits.global
import actors.VerifyActor
import play.api.libs.concurrent.Akka
import akka.actor.Props
import play.api.Play.current

object VerificationController extends Controller with ExtendedController {
  def sendVerifyMessage() = authAction()(parse.tolerantJson) {
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
          resOK()
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
                val identityUpdate = IdentityUpdate.create(email = Some(i.email.get.copy(isVerified = true)))
                i.update(identityUpdate)
                resOK("verified")
              } else {
                resUnauthorized("mail has changed")
              }
            }
            case VERIFY_TYPE_PHONENUMBER => {
              if (i.phoneNumber.map {
                _.toString
              }.getOrElse("").equalsIgnoreCase(vs.valueToBeVerified)) {
                val identityUpdate = IdentityUpdate.create(phoneNumber = Some(i.phoneNumber.get.copy(isVerified = true)))
                i.update(identityUpdate)
                resOK("verified")
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