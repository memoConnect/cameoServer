package actors

import akka.actor.{ Props, Actor }
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import models._
import constants.Messaging._
import scala.concurrent.Future
import helper.JsonHelper._
import play.api.libs.concurrent.Akka
import play.api.Play.current
import scala.Some

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 5:36 PM
 */
class SendMessageActor extends Actor {

  def receive = {

    case (message: Message, recipients: Seq[Recipient]) => {

      Logger.info("SendMessageActor: Processing message with id " + message.id)

      // get identity of sender
      Identity.find(message.fromIdentityId).map {
        case None => {
          val error = "Could not find fromIdentityID " + message.fromIdentityId
          Logger.error(error)
          new MessageStatus(message.fromIdentityId, MESSAGE_STATUS_ERROR, error)
        }
        case Some(fromIdentity: Identity) => {

          // create actors
          lazy val sendMailActor = Akka.system.actorOf(Props[SendMailActor])
          lazy val sendSmsActor = Akka.system.actorOf(Props[SendSmsActor])

          val futureMessageStatus: Seq[Future[MessageStatus]] = recipients.map {
            recipient =>
              {
                Logger.debug("SendMessageActor: Sending to recipient " + recipient.identityId)

                // dont send back to sender
                if (!recipient.identityId.equals(fromIdentity.id)) {
                  Identity.find(recipient.identityId).map {
                    case None => {
                      val error = "Could not find identityID " + recipient.identityId
                      Logger.error(error)
                      new MessageStatus(recipient.identityId, MESSAGE_STATUS_ERROR, error)
                    }
                    case Some(toIdentity) => {
                      toIdentity.preferredMessageType match {
                        case MESSAGE_TYPE_SMS   => sendSmsActor ! (message, fromIdentity, toIdentity, 0)
                        case MESSAGE_TYPE_EMAIL => sendMailActor ! (message, fromIdentity, toIdentity, 0)
                        case MESSAGE_TYPE_DEFAULT =>
                          // if recipient has a mail, send mail
                          if (toIdentity.email.isDefined) {
                            sendMailActor ! (message, fromIdentity, toIdentity, 0)
                          } else if (toIdentity.phoneNumber.isDefined) {
                            sendSmsActor ! (message, fromIdentity, toIdentity, 0)
                          }
                        // TODO case _ => sendFailActor ! (message, identity)
                      }
                      new MessageStatus(recipient.identityId, MESSAGE_STATUS_QUEUED, toIdentity.preferredMessageType)
                    }
                  }
                } else {
                  Future.successful(new MessageStatus(recipient.identityId, MESSAGE_STATUS_NONE, "sender"))
                }
              }
          }

          // convert to a singe future and write status to message
          Future.sequence(futureMessageStatus).map {
            s =>
              message.updateStatus(s)
          }
        }

      }
    }
  }
}