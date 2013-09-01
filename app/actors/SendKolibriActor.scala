package actors

import akka.actor.Actor
import traits.MongoHelper
import models.Recipient
import scala.Some
import controllers.MessageController
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import play.api.Logger

/**
 * User: Björn Reimer
 * Date: 8/30/13
 * Time: 8:24 PM
 */

class SendKolibriActor extends Actor with MongoHelper {

  def receive = {
    case (recipient: Recipient, message: models.Message) => {

      // we only need to add the conversation to the user, no real "sending" is involved
      val status = MessageController.addConversationToUser(message.conversationId.get, recipient.name).map {
//        case None => "Kolibri Message sent"
//        case Some(error) => "error"
        d => "d"
      }

      status.map {
        statusMessage => {
          Logger.info("SendKolibriActor: " + statusMessage)
        }
      }

      //      Recipient.updateRecipientStatus(message, recipient, messageStatus)
    }
  }
}
