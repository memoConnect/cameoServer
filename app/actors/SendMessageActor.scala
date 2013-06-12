package actors

import akka.actor.{Props, Actor}
import play.api.Logger
import helper.IdHelper
import play.api.libs.json._
import play.api.libs.json.Reads._
import traits.{MongoCollections, JsonTransformer}
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import play.api.libs.concurrent.Akka
import play.api.Play.current

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 5:36 PM
 */
class SendMessageActor extends Actor with JsonTransformer with MongoCollections {

  lazy val sendMailActor = Akka.system.actorOf(Props[SendMailActor], name = "sendMail")

  def sendMessage(user: JsObject, message: JsObject) = {
    val recipients: List[JsObject] = (message \ "recipients").asOpt[List[JsObject]].getOrElse(List())

    val recipientsWithStatus: JsObject = recipients.foldLeft(Json.obj())((newRecipients: JsObject,
      recipient: JsObject) => {
      def addRecipientWithStatus(status: String): JsObject = {
        recipient.transform(addStatus(status)).map {
          r => {
            newRecipients.transform(__.json.update((__ \ IdHelper.generateRecipientId()).json.put(r)))
          }.recoverTotal(error => {
            Logger.error("Error adding to recipients with status: ")
            newRecipients
          })
        }.recoverTotal(error => {
          Logger.error("Error adding status to recipient: ")
          newRecipients
        })
      }

      (recipient \ "messageType").asOpt[String].getOrElse("none") match {
        case "none" => addRecipientWithStatus("No MessageType given")
        case "email" => {
          val from = (user \ "email").asOpt[String].getOrElse("no email given")
          val to = (recipient \ "sendTo").asOpt[String].getOrElse("no email given")
          val subject = "Message from" + (user \ "name").asOpt[String].getOrElse("no name")
          val body = (message \ "messageBody").asOpt[String].getOrElse("empty Body")

          sendMailActor ! (from, to, subject, body)

          addRecipientWithStatus("Email not implemented yet")
        }
        case "sms" => addRecipientWithStatus("SMS not implemented yet")
        case m => addRecipientWithStatus("Unkown message type \'" + m + "\'")
      }
    })

    message.transform(__.json.update((__ \ 'recipients).json.put(recipientsWithStatus))).map {
      newMessage => {
        val messageId = (newMessage \ "messageId").asOpt[String].getOrElse("")
        val set = Json.obj("$set" -> Json.obj("messages." + messageId -> newMessage))
        val query = getConversationId(newMessage)

        conversationCollection.update(query, set).map {
          lastError => if (lastError.inError) {
            Logger.error("Error updating message: " + lastError.stringify)
          }
        }
      }
    }
  }


  def receive = {
    case message: JsObject => {
      Logger.info("SendMessageActor: Sending message with id " + (message \ "messageId").as[String])

      // get current user
      userCollection.find(getBranch(message, "username")).one[JsObject].map {
        case None => Logger.debug("none")
        case Some(user) => sendMessage(user, message)
      }
    }
  }
}