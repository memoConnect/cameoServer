package actors

import akka.actor.{ Actor, Props }
import constants.Verification._
import models.{ Account, Identity, VerificationSecret }
import play.api.Play
import play.api.Play.current
import play.api.i18n.Lang
import play.api.libs.concurrent.Akka
import services.LocalizationMessages

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 2/3/14
 * Time: 5:43 PM
 */

case class VerifyMail(account: Account, lang: Lang)
case class VerifySms(account: Account, lang: Lang)

class VerifyActor extends Actor {

  def receive = {

    case VerifyMail(account, lang) =>
      account.email match {
        case None => // do nothing
        case Some(email) =>
          val secret = VerificationSecret.create(account.id, email.value, VERIFY_TYPE_MAIL)
          VerificationSecret.insert(secret)

          val variables = Map (
          "link" -> (Play.configuration.getString("shortUrl.address").get + "/vr/" + secret.id),
          "code" ->  secret.getVerificationCode
          )

          val body = LocalizationMessages.get("VERIFICATION.MAIL.MESSAGES", lang, variables)
          val subject = LocalizationMessages.get("VERIFICATION.MAIL.SUBJECT", lang)
          val fromMail = Play.configuration.getString("verification.mail.from").get
          val from = "<" + fromMail ">" + LocalizationMessages.get("VERIFICATION.MAIL.SENDER", lang)

          lazy val sendMailActor = Akka.system.actorOf(Props[SendMailActor])
          sendMailActor ! Mail("Verification", from, email.value, body, subject)
      }

    case VerifySms(account, lang) =>
      account.phoneNumber match {
        case None => // do nothing
        case Some(phoneNumber) =>
          val secret = VerificationSecret.create(account.id, phoneNumber.value, VERIFY_TYPE_PHONENUMBER)
          VerificationSecret.insert(secret)

          val variables = Map(
            "link" -> (Play.configuration.getString("shortUrl.address").get + "/vr/" + secret.id),
            "code" -> secret.getVerificationCode
          )

          val body = LocalizationMessages.get("VERIFICATION.SMS.MESSAGES", lang, variables)
          val from = LocalizationMessages.get("VERIFICATION.MAIL.SENDER", lang)

          lazy val sendSmsActor = Akka.system.actorOf(Props[SendSmsActor])
          sendSmsActor ! Sms(from, phoneNumber.value, body)
      }
  }

}
