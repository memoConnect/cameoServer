package actors

import akka.actor.Actor
import play.api.{Play, Logger}
import play.api.Play.current
import play.api.libs.json.{JsString, Json}
import traits.{MongoHelper, JsonTransformer}
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import play.api.libs.ws.WS
import models.{User, Recipient}

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 8:01 PM
 */
class SendSMSActor extends Actor with JsonTransformer with MongoHelper {

  def receive = {
    case (recipient: Recipient, message: models.Message) => {
      // get user
      userCollection.find(Json.obj("username" -> message.from)).one[User].map {
        case Some(user) =>
          val from = user.name.getOrElse("Kolibrinet")
          val to = recipient.sendTo
          val body = message.messageBody

          // add footer to sms
          val footer = "... more: http://kl.vc/c/" + message.conversationId.getOrElse("")
          // cut message, so it will fit in the sms with the footer.
          val bodyWithFooter: String = {
            if (footer.length + body.length > 160) {
              body.substring(0, 160 - footer.length) + footer
            } else {
              body + footer
            }
          }

          Logger.info("SendSMSActor: Sending SMS to " + to + " from " + from)

          val postBody = Json.obj("api_key" -> JsString(Play.configuration.getString("nexmo.key").getOrElse("")),
            "api_secret" -> JsString(Play.configuration.getString("nexmo.secret").getOrElse("")), "from" -> from,
            "to" -> to, "text" -> bodyWithFooter)

          val response = WS.url(Play.configuration.getString("nexmo.url").getOrElse("")).post(postBody)

          response.map {
            nexmoResponse => {
              val status = {
                if (nexmoResponse.status < 300) {
                  val jsResponse = nexmoResponse.json
                  if ((jsResponse \ "status").asOpt[String].getOrElse("fail").equals("0")) {
                    "SMS Send. Id: " + (jsResponse \ "message-id").asOpt[String].getOrElse("none") + " Network:" +
                      (jsResponse \ "network").asOpt[String].getOrElse("none")


                  } else {
                    "Error sending SMS message: " + (jsResponse \ "error-text").asOpt[String].getOrElse("none")
                  }
                } else {
                  "Error connecting to Nexmo: " + nexmoResponse.statusText
                }
              }

              val query = Json.obj("conversationId" -> message.conversationId) ++ Json.obj("messages.messageId" +
                ".recipients.recipientId" -> recipient.recipientId)
              val set = Json.obj("$set" -> Json.obj("messages.recipients.$.status" -> status))

              conversationCollection.update(query, set).map {
                lastError => if (lastError.inError) {
                  Logger.error("Error updating recipient")
                }
              }
              Logger.info("SendSMSActor: " + status)

            }
          }
      }
    }
  }
}
