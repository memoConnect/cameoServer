package actors

import akka.actor.{ Actor, Props }
import constants.Messaging._
import models._
import play.api.Logger
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
          lazy val sendMailActor = Akka.system.actorOf(Props[SendMailActor])
          lazy val sendSmsActor = Akka.system.actorOf(Props[SendSmsActor])

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
                    // check if identity has an event subscription
                    EventSubscription.find(Json.obj("identityId" -> toIdentity.id)).map {
                      case Some(es) => // do nothing
                      case None =>
                        toIdentity.accountId match {
                          case None =>
                            // external user, use contacts from identity
                            if (toIdentity.phoneNumber.isDefined) {
                              sendSmsActor ! SmsWithPurl(message, fromIdentity, toIdentity, toIdentity.phoneNumber.get.value)
                            } else if (toIdentity.email.isDefined) {
                              sendMailActor ! MailWithPurl(message, fromIdentity, toIdentity, subject, toIdentity.email.get.value)
                            } else {
                              Logger.info("SendMessageActor: Identity " + toIdentity.id + " has no valid mail or sms")
                            }
                          case Some(accountId) =>
                            // internal user, use contacts from account
                            Account.find(accountId).map {
                              case Some(account) =>
                                if (account.phoneNumber.isDefined) {
                                  sendSmsActor ! SmsWithPurl(message, fromIdentity, toIdentity, account.phoneNumber.get)
                                } else if (account.email.isDefined) {
                                  sendMailActor ! MailWithPurl(message, fromIdentity, toIdentity, subject, account.email.get)
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