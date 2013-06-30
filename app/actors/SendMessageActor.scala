package actors

import akka.actor.{Props, Actor}
import play.api.Logger
import play.api.libs.json._
import play.api.libs.json.Reads._
import traits.{MongoHelper, JsonTransformer}
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import play.api.libs.concurrent.Akka
import play.api.Play.current
import models.{Recipient, Message}

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 5:36 PM
 */
class SendMessageActor extends Actor with JsonTransformer with MongoHelper {

  lazy val sendMailActor = Akka.system.actorOf(Props[SendMailActor], name = "sendMail")
  lazy val sendSMSActor = Akka.system.actorOf(Props[SendSMSActor], name = "sendSMS")

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
          } else {
            // check for message type
            recipient.messageType match {
              case "none" => recipientAddStatus("No MessageType given")
              case "email" => {
                sendMailActor ! (recipient, message)
                recipientAddStatus("Email queued")
              }
              case "sms" => {
                sendSMSActor !(recipient, message)
                recipientAddStatus("SMS queued")
              }
              case m => recipientAddStatus("Unkown message type \'" + m + "\'")
            }
          }
      }

      // add recipients with Status to message and save to db
      val query = Json.obj("conversationId" -> message.conversationId, "messages.messageId" -> message.messageId)
      val set = Json.obj("$set" -> Json.obj("messages.$.recipients" -> recipientsWithStatus.map(Recipient.toJson(_))))

      conversationCollection.update(query, set).map {
        lastError => if (lastError.inError) {
          Logger.error("Error updating message: " + lastError.stringify)
        }
      }

    }
  }
}