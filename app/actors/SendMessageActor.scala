package actors

import akka.actor.Actor
import play.api.Logger
import traits.MongoHelper
import play.api.libs.concurrent.Execution.Implicits._
import models.{MessageStatus, Identity, MongoId, Message}
import constants.Messaging._
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 5:36 PM
 */
class SendMessageActor extends Actor with MongoHelper {

  def receive = {
    case (message: Message, recipients: Seq[MongoId]) => {

      Logger.info("SendMessageActor: Processing message with id " + message.id)

      val futureMessageStatus: Seq[Future[MessageStatus]] = recipients.map {
        id => Identity.find(id).map {
          case None => {
            val error ="Could not find identityID " + id
            Logger.error(error)
            new MessageStatus(id, MESSAGE_STATUS_ERROR, error)
          }
          case Some(identity) => {
            identity.preferredMessageType match {
              case MESSAGE_TYPE_SMS => sendSMSActor !(message, identity, 0)
              case MESSAGE_TYPE_EMAIL => sendMailActor !(message, identity, 0)
              case MESSAGE_TYPE_DEFAULT => sendSMSActor !(message, identity, 0)
              // TODO case _ => sendFailActor ! (message, identity)
            }
            new MessageStatus(id, MESSAGE_STATUS_QUEUED, identity.preferredMessageType)
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