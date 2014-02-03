package controllers

import helper.AuthAction
import traits.ExtendedController
import play.api.mvc.Controller
import helper.ResultHelper._

import play.api.libs.json._
import play.api.libs.functional.syntax._

object VerificationController extends Controller with ExtendedController {
  def sendVerifyMessage() = AuthAction(parse.tolerantJson) {
    request =>
      case class VerifyRequest(verifyPhoneNumber: Option[Boolean], verifyMail: Option[Boolean])

      val reads = (
        (__ \ "verifyPhoneNumber").readNullable[Boolean] and
          (__ \ "verifyEmail").readNullable[Boolean]
        )(VerifyRequest.apply _)

      request.body.validate[VerifyRequest](reads).map {
        vr => if()


          Ok("")
      }.recoverTotal(e => BadRequest(resKO(JsError.toFlatJson(e))))


  }
}