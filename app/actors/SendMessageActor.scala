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

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 5:36 PM
 */
class SendMessageActor extends Actor with JsonTransformer with MongoHelper {

  lazy val sendMailActor = Akka.system.actorOf(Props[SendMailActor], name = "sendMail")

  def sendMessage(user: JsObject, message: JsObject) = {

    val recipients: JsObject = (message \ "recipients").asOpt[JsObject].getOrElse({
      Logger.error("error adding status to recipients");
      Json.obj()
    })

    val recipientsWithStatus  = JsObject(recipients.fields.map {
      case (recipientId: String, recipient: JsObject) =>

        def addRecipientStatus(status: String): (String, JsObject) = {
          (recipientId, recipient.transform(__.json.update((__ \ 'status).json.put(JsString(status)))).get)
        }

        // check for message type
        (recipient \ "messageType").asOpt[String].getOrElse("none") match {
          case "none" => addRecipientStatus("No MessageType given")
          case "email" => {
            sendMailActor !(recipient, message, user)
            addRecipientStatus("Email queued")
          }
          case "sms" => addRecipientStatus("SMS not implemented yet")
          case m => addRecipientStatus("Unkown message type \'" + m + "\'")
        }
    })

    // add recipients with Status to message and save to db
    val messageId = (message \ "messageId").asOpt[String].getOrElse("")
    val set = Json.obj("$set" -> Json.obj("messages." + messageId + ".recipients" -> recipientsWithStatus))
    val query = getConversationId(message)

    conversationCollection.update(query, set).map {
      lastError => if (lastError.inError) {
        Logger.error("Error updating message: " + lastError.stringify)
      }
    }


  }

  def receive = {
    case message: JsObject => {
      Logger.info("SendMessageActor: Sending message with id " + (message \ "messageId").as[String])

      // get current user
      userCollection.find(getBranch(message, "username")).one[JsObject].map {
        case Some(user) => sendMessage(user, message)
      }
    }
  }
}