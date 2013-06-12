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
      // add recipient id
      val recipientId = IdHelper.generateRecipientId()

      recipient.transform(__.json.update((__ \ 'recipientId).json.put(JsString(recipientId)))).map {
        recipientWithId => {
          def addRecipientWithStatus(status: String): JsObject = {
            recipientWithId.transform(addStatus(status)).map {
              recipientWithStatus =>
                newRecipients.transform(__.json.update((__ \ recipientId).json.put(recipientWithStatus))).get
            }.recoverTotal(error => {
              Logger.error("Error adding status to recipient")
              newRecipients
            })
          }

          (recipientWithId \ "messageType").asOpt[String].getOrElse("none") match {
            case "none" => addRecipientWithStatus("No MessageType given")
            case "email" => {
              sendMailActor !(recipientWithId, message, user)
              addRecipientWithStatus("Email queued")
            }
            case "sms" => addRecipientWithStatus("SMS not implemented yet")
            case m => addRecipientWithStatus("Unkown message type \'" + m + "\'")
          }
        }
      }.recoverTotal(error => {
        Logger.error("Error adding id to recipient")
        newRecipients
      })
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