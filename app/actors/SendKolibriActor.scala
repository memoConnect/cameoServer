package actors

import akka.actor.Actor
import traits.MongoHelper
import models.{User, Recipient}
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

      Logger.debug("HERE")

      // we only need to add the conversation to the user, no real "sending" is involved
      val status = User.addConversation(message.conversationId.get, recipient.sendTo).map {
        res => if (res) {
          "Kolibri-Message send"
        } else {
          "Error sending message"
        }
      }

      status.map {
        statusMessage => {
          Recipient.updateStatus(message, recipient, statusMessage)
          Logger.info("SendKolibriActor: " + statusMessage)
        }
      }

    }
  }
}
