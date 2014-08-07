package actors

import akka.actor.{ Actor, Props }
import constants.Messaging._
import models._
import play.api.{ Play, Logger }
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.Json

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 5:36 PM
 */

case class Notification(message: Message, conversationId: MongoId, recipients: Seq[Recipient], subject: String)

class NotificationActor extends Actor {

  def receive = {

    case Notification(message, conversationId, recipients, subject) =>

      //      Logger.info("SendMessageActor: Processing message with id " + message.id)

      // get identity of sender
      Identity.find(message.fromIdentityId).map {
        case None =>
          Logger.error("Could not find fromIdentityID " + message.fromIdentityId)
        case Some(fromIdentity: Identity) =>

          // create actors
          lazy val sendMailActor = Akka.system.actorOf(SendMailActorProps)
          lazy val sendSmsActor = Akka.system.actorOf(SendSmsActorProps)

          recipients.map {
            recipient =>
              // send event
              eventRouter ! NewMessage(recipient.identityId, conversationId, message)

              // dont send back to sender
              if (!recipient.identityId.equals(fromIdentity.id)) {
                Identity.find(recipient.identityId).map {
                  case None =>
                    val error = "Could not find identityID " + recipient.identityId
                    Logger.error(error)
                  case Some(toIdentity) =>
                    val supportIdentityId = Play.configuration.getString("support.contact.identityId").getOrElse("")

                    // check if identity has an event subscription
                    EventSubscription.find(Json.obj("identityId" -> toIdentity.id)).map {
                      case Some(es)                                               => // do nothing
                      case Some(es) if es.identityId.id.equals(supportIdentityId) => sendNotification()
                      case None                                                   => sendNotification()
                    }

                    def sendNotification() {
                      toIdentity.accountId match {
                        case None =>
                          // external user, use contacts from identity
                          if (toIdentity.phoneNumber.isDefined) {
                            sendSmsActor ! generateSms(message, fromIdentity, toIdentity, toIdentity.phoneNumber.get.value)
                          } else if (toIdentity.email.isDefined) {
                            sendMailActor ! generateMail(message, fromIdentity, toIdentity, subject, toIdentity.email.get.value)
                          } else {
                            Logger.info("SendMessageActor: Identity " + toIdentity.id + " has no valid mail or sms")
                          }
                        case Some(accountId) =>
                          // internal user, use contacts from account
                          Account.find(accountId).map {
                            case Some(account) =>
                              if (account.phoneNumber.isDefined) {
                                sendSmsActor ! generateSms(message, fromIdentity, toIdentity, account.phoneNumber.get.value)
                              } else if (account.email.isDefined) {
                                sendMailActor ! generateMail(message, fromIdentity, toIdentity, subject, account.email.get.value)
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

  def generateMail(message: models.Message, fromIdentity: Identity, toIdentity: Identity, subject: String, email: String): Mail = {
    // get identity of sender
    val fromName = fromIdentity.displayName.getOrElse(fromIdentity.cameoId)
    val from: String = fromName + "<" + Play.configuration.getString("mail.from").get + ">"
    val mailSubject = "[cameo.io] - " + subject
    val to: String = email
    val body: String = message.plain match {
      case Some(PlainMessagePart(Some(text), _)) => text
      case _                                     => MESSAGE_TEXT_REPLACE_ENCRYPTED
    }

    // create purl
    val purl = Purl.create(message.id, toIdentity.id)
    Purl.col.insert(purl)

    // add footer to mail
    val footer = "\n\n---\nRead entire conversation on cameonet.de: " + Play.configuration.getString("shortUrl.address").get + "/p/" + purl.id

    // cut message, so it will fit in the sms with the footer.
    val bodyWithFooter = body + footer

    new Mail(from, to, bodyWithFooter, mailSubject)
  }

  def generateSms(message: Message, fromIdentity: Identity, toIdentity: Identity, phoneNumber: String): Sms = {
    val from: String = fromIdentity.displayName.getOrElse(fromIdentity.cameoId)
    val to: String = phoneNumber
    val body: String = message.plain match {
      case Some(PlainMessagePart(Some(text), _)) => text
      case _                                     => MESSAGE_TEXT_REPLACE_ENCRYPTED
    }

    // create purl
    val purl = Purl.create(message.id, toIdentity.id)
    Purl.col.insert(purl)

    // add footer to sms
    val footer = "... more: " + Play.configuration.getString("shortUrl.address").getOrElse("none") + "/p/" + purl.id

    // cut message, so it will fit in the sms with the footer.
    val bodyWithFooter: String = {
      if (footer.length + body.length > 160) {
        body.substring(0, 160 - footer.length) + footer
      } else {
        body + footer
      }
    }

    // check if we have a test user and save message
    val testUserPrefix = Play.configuration.getString("testUser.prefix").getOrElse("foo")
    toIdentity.cameoId.startsWith(testUserPrefix) match {
      case false =>
      case true  => TestUserNotification.createAndInsert(toIdentity.id, "sms", bodyWithFooter, false)
    }

    // check if this message comes from a test user and save it
    fromIdentity.cameoId.startsWith(testUserPrefix) match {
      case false =>
      case true  => TestUserNotification.createAndInsert(fromIdentity.id, "sms", bodyWithFooter, true)
    }

    new Sms(from, to, bodyWithFooter)
  }
}