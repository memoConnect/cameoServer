package actors

import akka.actor.Actor
import constants.Confirmation
import constants.Confirmation._
import models._
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

case class ConfirmMail(accountId: MongoId, lang: Lang, confirmationToken: Option[ConfirmationToken] = None)
case class ConfirmPhoneNumber(accountId: MongoId, lang: Lang, confirmationToken: Option[ConfirmationToken]= None)

trait ConfirmationActor extends Actor {

  val confirmationType: String
  val shortUrlPath: String
  val mailFromAddress: String

  // Language Keys
  val mailSender: String
  val mailMessage: String
  val mailSubject: String
  val smsSender: String
  val smsMessage: String

  def receive = {

    case ConfirmMail(accountId, lang, ct) =>
      Account.find(accountId).map {
        case None => // do nothing
        case Some(account) =>
          account.email match {
            case None => // do nothing
            case Some(email) =>
              val confirmation = ct.getOrElse(ConfirmationToken.createAndInsert(account.id, confirmationType, CONFIRMATION_PATH_MAIL, email.value))

              val variables = Map(
                "link" -> (Play.configuration.getString("shortUrl.address").get + shortUrlPath +"/" + confirmation.id),
                "code" -> confirmation.code
              )

              val body = LocalizationMessages.get(mailMessage, lang, variables)
              val subject = LocalizationMessages.get(mailSubject, lang)

              // check if we have a test user and save message
              val testUserPrefix = Play.configuration.getString("testUser.prefix").getOrElse("foo")
              if (account.loginName.startsWith(testUserPrefix.toLowerCase))  {
                TestUserNotification.createAndInsert(account.id, "email", body, false)
              }

              lazy val sendMailActor = Akka.system.actorOf(actors.SendMailActorProps)
              sendMailActor ! Mail(LocalizationMessages.get(mailSender, lang), mailFromAddress, email.value, body, subject)
          }
      }

    case ConfirmPhoneNumber(accountId, lang, ct) =>
      Account.find(accountId).map {
        case None => // do nothing
        case Some(account) =>
          account.phoneNumber match {
            case None => // do nothing
            case Some(phoneNumber) =>
              val confirmation = ct.getOrElse(ConfirmationToken.createAndInsert(account.id, confirmationType, CONFIRMATION_PATH_PHONENUMBER, phoneNumber.value))

              val variables = Map(
                "link" -> (Play.configuration.getString("shortUrl.address").get + shortUrlPath +"/" + confirmation.id),
                "code" -> confirmation.code
              )

              val body = LocalizationMessages.get(smsMessage, lang, variables)
              val from = LocalizationMessages.get(smsSender, lang)

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

class VerificationActor extends ConfirmationActor {
  val confirmationType: String = Confirmation.CONFIRMATION_TYPE_VERIFICATION
  val shortUrlPath: String = "/vr"
  val mailFromAddress: String = Play.configuration.getString("verification.mail.from").get

  val mailSender: String = "BACKEND.VERIFICATION.MAIL.SENDER"
  val mailMessage: String = "BACKEND.VERIFICATION.MAIL.MESSAGE"
  val mailSubject: String = "BACKEND.VERIFICATION.MAIL.SUBJECT"
  val smsMessage: String = "BACKEND.VERIFICATION.SMS.MESSAGE"
  val smsSender: String = "BACKEND.VERIFICATION.SMS.SENDER"
}


class ResetPasswordActor extends ConfirmationActor {
  val confirmationType: String = Confirmation.CONFIRMATION_TYPE_RESET_PASSWORD
  val shortUrlPath: String = "/pr"
  val mailFromAddress: String = Play.configuration.getString("password.reset.mail.from").get

  val mailSender: String = "BACKEND.RESET_PASSWORD.MAIL.SENDER"
  val mailMessage: String = "BACKEND.RESET_PASSWORD.MAIL.MESSAGE"
  val mailSubject: String = "BACKEND.RESET_PASSWORD.MAIL.SUBJECT"
  val smsMessage: String = "BACKEND.RESET_PASSWORD.SMS.MESSAGE"
  val smsSender: String = "BACKEND.RESET_PASSWORD.SMS.SENDER"
}
