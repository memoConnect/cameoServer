package actors

import akka.actor.Actor
import constants.Messaging._
import events.{ ConversationNewMessageWithPush, ConversationNewMessage }
import models._
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.Json
import play.api.{ Logger, Play }

import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 5:36 PM
 */

case class ExternalMessage(message: Message, conversation: Conversation)

class ExternalMessageActor extends Actor {

  def receive = {

    case ExternalMessage(message, conversation) =>

      // get identity of sender
      Identity.find(message.fromIdentityId).map {
        case None =>
          Logger.error("Could not find fromIdentityID " + message.fromIdentityId)
        case Some(fromIdentity: Identity) =>

          // create actors
          lazy val sendMailActor = Akka.system.actorOf(SendMailActorProps)
          lazy val sendSmsActor = Akka.system.actorOf(SendSmsActorProps)

          conversation.recipients.map {
            recipient =>
              Identity.find(recipient.identityId).map {
                case None => Logger.error("DBError: Could not find identityID " + recipient.identityId)
                case Some(recipientIdentity) =>
                  // get account, if identity has one
                  recipientIdentity.accountId.fold[Future[Option[Account]]](Future(None))(Account.find).map {
                    maybeAccount =>
                      val conversationWithMessage = conversation.copy(messages = conversation.messages :+ message)
                      // don't send external message to sender
                      if (recipient.identityId.equals(fromIdentity.id)) {
                        eventRouter ! ConversationNewMessage(recipient.identityId, conversationWithMessage, message, maybeAccount.map(_.userSettings))
                      } else {
                        eventRouter ! ConversationNewMessageWithPush(recipient.identityId, fromIdentity, conversationWithMessage, message, maybeAccount.map(_.userSettings))

                        // check if identity has an event subscription or is the support
                        val supportIdentityId = Play.configuration.getString("support.contact.identityId").getOrElse("")
                        EventSubscription.find(Json.obj("identityId" -> recipientIdentity.id)).map {
                          case None                                                   => sendExternalEvents()
                          case Some(es) if es.identityId.id.equals(supportIdentityId) => sendExternalEvents()
                          case Some(es)                                               => Logger.info("Not sending external Message " + message.id.id + " to identity " + recipientIdentity.id.id + ". There is an event subscription")
                        }

                        def sendExternalEvents() {
                          recipientIdentity.accountId match {
                            case None =>
                              // external user, use contacts from identity
                              if (recipientIdentity.phoneNumber.isDefined) {
                                sendSmsActor ! generateSms(message, fromIdentity, recipientIdentity, recipientIdentity.phoneNumber.get.value)
                              } else if (recipientIdentity.email.isDefined) {
                                sendMailActor ! generateMail(message, fromIdentity, recipientIdentity, conversation.subject.getOrElse("no subject"), recipientIdentity.email.get.value)
                              } else {
                                Logger.info("SendMessageActor: Identity " + recipientIdentity.id + " has no valid mail or sms")
                              }
                            case Some(accountId) =>
                              // internal user, use contacts from account
                              Account.find(accountId).map {
                                case Some(account) =>
                                  if (account.phoneNumber.isDefined) {
                                    sendSmsActor ! generateSms(message, fromIdentity, recipientIdentity, account.phoneNumber.get.value)
                                  } else if (account.email.isDefined) {
                                    sendMailActor ! generateMail(message, fromIdentity, recipientIdentity, conversation.subject.getOrElse("no subject"), account.email.get.value)
                                  } else {
                                    Logger.info("SendMessageActor: Account " + account.id + " has no valid mail or sms")
                                  }
                                case None => Logger.error("Could not find accountId: " + accountId)
                              }
                          }
                        }

                      }
                  }
              }
          }
      }
  }

  def generateMail(message: models.Message, fromIdentity: Identity, toIdentity: Identity, subject: String, email: String): Mail = {

    // create purl
    val purl = Purl.create(message.id, toIdentity.id)
    Purl.col.insert(purl)
    val url = Play.configuration.getString("shortUrl.address").get + "/p/" + purl.id

    // get identity of sender
    val fromName = fromIdentity.displayName.getOrElse(fromIdentity.cameoId)
    val fromMail = Play.configuration.getString("mail.from").get
    val mailSubject = "[cameo.io] - " + subject
    val to: String = email

    val body: String = message.plain match {
      case Some(PlainMessagePart(Some(text), _)) =>
        text + "\n\n---\nRead the entire conversation and answer on cameoNet.de: " + url
      case Some(PlainMessagePart(None, files)) if files.isDefined && files.get.length > 0 =>
        MESSAGE_MAIL_FILE_ONLY_EN + url + "\n\n" + MESSAGE_MAIL_FILE_ONLY_DE + url
      case _ =>
        MESSAGE_MAIL_REPLACE_ENCRYPTED_EN + url + "\n\n" + MESSAGE_MAIL_REPLACE_ENCRYPTED_DE + url
    }

    new Mail(fromName, fromMail, to, body, mailSubject)
  }

  def generateSms(message: Message, fromIdentity: Identity, toIdentity: Identity, phoneNumber: String): Sms = {

    // create purl
    val purl = Purl.create(message.id, toIdentity.id)
    Purl.col.insert(purl)
    val url = Play.configuration.getString("shortUrl.address").get + "/p/" + purl.id

    val from: String = fromIdentity.displayName.getOrElse(fromIdentity.cameoId)
    val to: String = phoneNumber

    def shortenBody(body: String): String = {
      val footer = "... read more: " + url
      if (footer.length + body.length > 160) {
        body.substring(0, 160 - footer.length) + footer
      } else {
        body + footer
      }
    }

    val body: String = message.plain match {
      case Some(PlainMessagePart(Some(text), _))                                          => shortenBody(text + " ")
      case Some(PlainMessagePart(None, files)) if files.isDefined && files.get.length > 0 => MESSAGE_SMS_FILE_ONLY + url
      case _                                                                              => MESSAGE_SMS_REPLACE_ENCRYPTED + url
    }

    // check if we have a test user and save message
    val testUserPrefix = Play.configuration.getString("testUser.prefix").getOrElse("foo")
    toIdentity.cameoId.startsWith(testUserPrefix) match {
      case false =>
      case true  => TestUserNotification.createAndInsert(toIdentity.id, "sms", body, false)
    }

    // check if this message comes from a test user and save it
    fromIdentity.cameoId.startsWith(testUserPrefix) match {
      case false =>
      case true  => TestUserNotification.createAndInsert(fromIdentity.id, "sms", body, true)
    }

    new Sms(from, to, body)
  }
}