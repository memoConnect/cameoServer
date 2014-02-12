package actors

import akka.actor.{ Props, Actor }
import play.api.libs.concurrent.Execution.Implicits._
import play.api.Logger
import play.api.libs.concurrent.Akka
import play.api.Play.current
import helper.MongoHelper._

/**
 * User: Björn Reimer
 * Date: 8/30/13
 * Time: 8:24 PM
 */

class SendKolibriActor extends Actor {
  //TODO implementation missing
  def receive = {
    case (message: models.Message) => {

      //      // we only need to add the conversation to the user, no real "sending" is involved
      //      val status = User.addConversation(message.conversationId.get, recipient.sendTo).map {
      //        res => if (res) {
      //          "Kolibri-Message send"
      //        } else {
      //          "Error sending message"
      //        }
      //      }
      //
      //      status.map {
      //        statusMessage => {
      //          Recipient.updateStatus(message, recipient, statusMessage)
      //
      //          // send notification to user
      //          notificationActor ! (recipient.sendTo, message)
      //
      //          Logger.info("SendKolibriActor: " + statusMessage)
      //        }
      //      }
      //
      //    }
    }
  }
}