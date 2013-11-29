package actors

import akka.actor.{Props, Actor}
import play.api.Logger
import play.api.libs.json._
import traits.MongoHelper
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.concurrent.Akka
import play.api.Play.current
import models.{Recipient, Message}
import java.util.Date

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 5:36 PM
 */
class SendMessageActor extends Actor with MongoHelper {

  def receive = {
    case message: Message => {
      Logger.info("SendMessageActor: Sending message with id " + message.messageId)

      val recipients = message.recipients.getOrElse(Seq())

      val recipientsWithStatus = recipients.map {
        recipient: Recipient =>

          def recipientAddStatus(status: String) = {
            recipient.copy(sendStatus = Some(status))
          }

          // check if we have a test run
          if (recipient.testRun.getOrElse(false)) {
            recipientAddStatus("testrun: message not send")
            // do not send the message to the sender
          } else if (message.fromRecipientId.getOrElse("none").equals(recipient.recipientId) || ( recipient.messageType.equals("otherUser") && message.from.equals(recipient.sendTo) )) {
            recipientAddStatus("Sender of message")
          }
          else {
            // check for message type
            recipient.messageType match {
              case "none" => recipientAddStatus("No MessageType given")
              case "email" => {
                sendMailActor ! (recipient, message)
                recipientAddStatus("Email queued")
              }
              case "sms" => {
                sendSMSActor ! (recipient, message)
                recipientAddStatus("SMS queued")
              }
              case "otherUser" => {
                sendKolibriActor ! (recipient, message)
                recipientAddStatus("KolibriMessage queued")
              }
              case m => recipientAddStatus("Unkown message type \'" + m + "\'")
            }
          }
      }

      // add recipients with Status to message and save to db
      val query = Json.obj("conversationId" -> message.conversationId, "messages.messageId" -> message.messageId)
      val set = Json.obj("$set" -> (
        Json.obj("messages.$.recipients" -> recipientsWithStatus.map(Recipient.toJson)) ++
          Json.obj("lastUpdated" -> Json.obj("$date" -> new Date)) ++
          Json.obj("lastMessage" -> Json.toJson(message))
        ))

      conversationCollection.update(query, set).map {
        lastError => if (lastError.inError) {
          Logger.error("Error updating message: " + lastError.stringify)
        }
      }
    }
  }
}