package actors

import akka.actor.{ Props, Actor }
import play.api.{ Play, Logger }
import play.api.libs.concurrent.Execution.Implicits._
import models._
import constants.Messaging._
import scala.concurrent.Future
import play.api.libs.concurrent.Akka
import play.api.Play.current
import scala.Some

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 5:36 PM
 */

case class SendMessage(message: Message, conversationId: MongoId, recipients: Seq[Recipient], subject: String)

class SendMessageActor extends Actor {

  def receive = {

    case SendMessage(message, conversationId, recipients, subject) =>

      Logger.info("SendMessageActor: Processing message with id " + message.id)

      // get identity of sender
      Identity.find(message.fromIdentityId).map {
        case None =>
          val error = "Could not find fromIdentityID " + message.fromIdentityId
          Logger.error(error)
          new MessageStatus(message.fromIdentityId, MESSAGE_STATUS_ERROR, error)
        case Some(fromIdentity: Identity) =>

          // create actors
          lazy val sendMailActor = Akka.system.actorOf(Props[SendMailActor])
          lazy val sendSmsActor = Akka.system.actorOf(Props[SendSmsActor])

          val futureMessageStatus: Seq[Future[MessageStatus]] = recipients.map {
            recipient =>
              {
                // send event
                eventRouter ! NewMessage(recipient.identityId, conversationId, message)

                // dont send back to sender
                if (!recipient.identityId.equals(fromIdentity.id)) {
                  Identity.find(recipient.identityId).map {
                    case None =>
                      val error = "Could not find identityID " + recipient.identityId
                      Logger.error(error)
                      new MessageStatus(recipient.identityId, MESSAGE_STATUS_ERROR, error)
                    case Some(toIdentity) =>
                      Logger.debug("SendMessageActor: Message " + message.id + " Sending to identity " + toIdentity.id)

                      toIdentity.accountId match {
                        case None =>
                          // external user, use contacts from identity
                          if (toIdentity.phoneNumber.isDefined) {
                            sendSmsActor ! (message, fromIdentity, toIdentity, toIdentity.phoneNumber.get.value, 0)
                          } else if (toIdentity.email.isDefined) {
                            sendMailActor ! (message, fromIdentity, toIdentity, subject, toIdentity.email.get.value, 0)
                          } else {
                            Logger.info("SendMessageActor: Identity " + toIdentity.id + " has no valid mail or sms")
                          }
                        case Some(accountId) =>
                          // internal user, use contacts from account
                          Account.find(accountId).map {
                            case Some(account) =>
                              if (account.phoneNumber.isDefined) {
                                sendSmsActor ! (message, fromIdentity, toIdentity, account.phoneNumber.get, 0)
                              } else if (account.email.isDefined) {
                                sendMailActor ! (message, fromIdentity, toIdentity, subject, account.email.get, 0)
                              } else {
                                Logger.info("SendMessageActor: Account " + account.id + " has no valid mail or sms")
                              }
                            case None => Logger.error("Could not find accountId: " + accountId)
                          }
                      }
                      new MessageStatus(recipient.identityId, MESSAGE_STATUS_QUEUED, toIdentity.preferredMessageType)
                  }
                } else {
                  Future.successful(new MessageStatus(recipient.identityId, MESSAGE_STATUS_NONE, "sender"))
                }
              }
          }
      }
  }
}