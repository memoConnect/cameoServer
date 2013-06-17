package actors

import akka.actor.Actor
import play.api.{Play, Logger}
import play.api.Play.current
import play.api.libs.json.{JsString, Json, JsObject}
import traits.{MongoHelper, JsonTransformer}
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import play.api.libs.ws.WS

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 8:01 PM
 */
class SendSMSActor extends Actor with JsonTransformer with MongoHelper {

  def receive = {
    case (recipient: JsObject, message: JsObject, user: JsObject) => {
      val from = (user \ "name").asOpt[String].getOrElse("KolibriNet User")
      val to = (recipient \ "sendTo").asOpt[String].getOrElse("no number")
      val body = (message \ "messageBody").asOpt[String].getOrElse("empty Message")
      val messageId = (message \ "messageId").asOpt[String].getOrElse("")
      val recipientId = (recipient \ "recipientId").asOpt[String].getOrElse("")
      val conversationId = (message \ "conversationId").asOpt[String].getOrElse("")

      // add footer to sms
      val footer = "... more: http://kl.vc/c/" + conversationId
      // cut message, so it will fit in the sms with the footer.
      val bodyWithFooter = body.substring(0, 160 - footer.length) + footer

      Logger.info("SendSMSActor: Sending SMS to " + to + " from " + from)

      val postBody = Json.obj("api_key" -> JsString(Play.configuration.getString("nexmo.key").getOrElse("")), "api_secret" -> JsString(Play.configuration.getString("nexmo.secret").getOrElse("")), "from" -> from, "to" -> to, "text" -> bodyWithFooter)

      val response = WS.url(Play.configuration.getString("nexmo.url").getOrElse("")).post(postBody)

      var status = ""

      response.map {
        nexmoResponse => {
          if (nexmoResponse.status < 300) {
            val jsResponse = nexmoResponse.json
            if ((jsResponse \ "status").asOpt[String].getOrElse("fail").equals("0")) {
              status = "SMS Send. Id: " + (jsResponse \ "message-id").asOpt[String].getOrElse("none") + " Network:" + (jsResponse \ "network").asOpt[String].getOrElse("none")
            } else {
              status = "Error sending message: " + (jsResponse \ "error-text").asOpt[String].getOrElse("none")
            }
          } else {
            status = "Error connecting to Nexmo: " + nexmoResponse.statusText
          }
        }
      }

      val query = Json.obj("messages." + messageId -> Json.obj("$exists" -> true))
      val set = Json.obj("$set" -> Json.obj("messages." + messageId + ".recipients." + recipientId + ".status" ->
        JsString(status)))

      conversationCollection.update(query, set).map {
        lastError => if (lastError.inError) {
          Logger.error("Error updating recipient")
        }
      }

      Logger.info("SendSMSActor: " + status)
    }
  }

}
