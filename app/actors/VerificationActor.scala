package actors

import akka.actor.Actor
import constants.Verification._
import models.{TestUserNotification, Account, MongoId, VerificationSecret}
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

case class VerifyMail(accountId: MongoId, lang: Lang)
case class VerifyPhoneNumber(accountId: MongoId, lang: Lang)

class VerificationActor extends Actor {

  def receive = {

    case VerifyMail(accountId, lang) =>
      Account.find(accountId).map {
        case None => // do nothing
        case Some(account) =>
          account.email match {
            case None => // do nothing
            case Some(email) =>
              val secret = VerificationSecret.createAndInsert(account.id, email.value, VERIFY_TYPE_MAIL)

              val variables = Map(
                "link" -> (Play.configuration.getString("shortUrl.address").get + "/vr/" + secret.id),
                "code" -> secret.code
              )

              val body = LocalizationMessages.get("BACKEND.VERIFICATION.MAIL.MESSAGE", lang, variables)
              val subject = LocalizationMessages.get("BACKEND.VERIFICATION.MAIL.SUBJECT", lang)
              val fromMail = Play.configuration.getString("verification.mail.from").get

              lazy val sendMailActor = Akka.system.actorOf(actors.SendMailActorProps)
              sendMailActor ! Mail(LocalizationMessages.get("BACKEND.VERIFICATION.MAIL.SENDER", lang), fromMail, email.value, body, subject)
          }
      }

    case VerifyPhoneNumber(accountId, lang) =>
      Account.find(accountId).map {
        case None => // do nothing
        case Some(account) =>
          account.phoneNumber match {
            case None => // do nothing
            case Some(phoneNumber) =>
              val secret = VerificationSecret.createAndInsert(account.id, phoneNumber.value, VERIFY_TYPE_PHONENUMBER)

              val variables = Map(
                "link" -> (Play.configuration.getString("shortUrl.address").get + "/vr/" + secret.id),
                "code" -> secret.code
              )

              val body = LocalizationMessages.get("BACKEND.VERIFICATION.SMS.MESSAGE", lang, variables)
              val from = LocalizationMessages.get("BACKEND.VERIFICATION.SMS.SENDER", lang)

              // check if we have a test user and save message
              val testUserPrefix = Play.configuration.getString("testUser.prefix").getOrElse("foo")
              if (account.loginName.startsWith(testUserPrefix.toLowerCase))  {
                TestUserNotification.createAndInsert(account.id, "sms", body, false)
              }

              lazy val sendSmsActor = Akka.system.actorOf(actors.SendSmsActorProps)
              sendSmsActor ! Sms(from, phoneNumber.value, body)
          }
      }
  }
}
