package actors

import akka.actor.Actor
import play.api.{Play, Logger}
import play.api.Play.current
import play.api.libs.json.{JsValue, Json}
import traits.MongoHelper
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws.WS
import scala.concurrent.Future
import models._
import constants.Messaging._
import play.api.libs.json.JsString


/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 8:01 PM
 */
class SendSMSActor extends Actor with MongoHelper {

  def sendSMS(from: String, to: String, body: String): Future[MessageStatus] = {

    Logger.debug("SendSMSActor: To: " + to + " Content: " + body)

    val postBody = Json.obj("api_key" -> JsString(Play.configuration.getString("nexmo.key").getOrElse("")),
      "api_secret" -> JsString(Play.configuration.getString("nexmo.secret").getOrElse("")), "from" -> from,
      "to" -> to, "text" -> body)

    val response = WS.url(Play.configuration.getString("nexmo.url").getOrElse("")).post(postBody)

    response.map {
      nexmoResponse => {
        val messages: Seq[MessageStatus] = {
          if (nexmoResponse.status < 300) {

            val jsResponse = nexmoResponse.json

            val messageReports = (jsResponse \ "messages").asOpt[Seq[JsValue]].getOrElse(Seq())

            messageReports.map {
              report => {
                if ((report \ "status").asOpt[String].get.equals("0")) {
                  val s = "SMS Send. Id: " + (report \ "message-id").asOpt[String].getOrElse("none") + " Network:" +
                    (report \ "network").asOpt[String].getOrElse("none")
                  new MessageStatus(new MongoId(""), MESSAGE_STATUS_SEND, s)
                } else {
                  val s = "Error sending SMS message. Response: " + jsResponse.toString
                  new MessageStatus(new MongoId(""), MESSAGE_STATUS_ERROR, s)
                }
              }
            }
          } else {
            val s = "Error connecting to Nexmo: " + nexmoResponse.statusText
            Seq(new MessageStatus(new MongoId(""), MESSAGE_STATUS_ERROR, s))
          }
        }

        val message = messages.head
        Logger.info("SendSMSActor: Sent SMS to " + to + " from " + from + " STATUS: " + message)
        message
      }
    }
  }

  def receive = {
    // send message to recipient
    case (message: Message, fromIdentity: Identity, identity: Identity, tryCount: Int) => {

      // check how offen we tried to send this message
      if (tryCount > MESSAGE_MAX_TRY_COUNT) {
        val ms = new MessageStatus(identity.id, MESSAGE_STATUS_ERROR, "max try count reached")
        Logger.debug("####################################################")
        // TODO update status of single message
        //message.updateStatus(Seq(ms))
      } else if (identity.phoneNumber.isEmpty) {
        // Do something else (send mail maybe
      } else {

        // get identity of sender
        val from: String = fromIdentity.displayName.getOrElse(IDENTITY_DEFAULT_DISPLAY_NAME)
        val to: String = identity.phoneNumber.get
        val body: String = message.messageBody

        // add footer to sms
        val footer = "... more: " + Play.configuration.getString("shortUrl.address").getOrElse("none") + "/p/"
        //+  Purl.createPurl(message.conversationId.get, recipient)
        // cut message, so it will fit in the sms with the footer.
        val bodyWithFooter: String = {
          if (footer.length + body.length > 160) {
            body.substring(0, 160 - footer.length) + footer
          } else {
            body + footer
          }
        }

        sendSMS(from, to, bodyWithFooter).map {
          statusMessage => {
            if (statusMessage.status.equals(MESSAGE_STATUS_SEND)) {
              // WOO
            } else {
              // try again
              sendSMSActor !(message, fromIdentity, identity, tryCount + 1)
            }
          }
        }


      }
    }



    // notify user of new message
    //    case (user: User, message: Message) => {
    //
    //      val from = message.from
    //      val to = user.phonenumber.getOrElse("none")
    //      val body = message.messageBody
    //
    //      // add footer to sms
    //      val footer = "... more: " + Play.configuration.getString("shortUrl.address").getOrElse("none") + "/p/" +
    //        Purl.createPurl(message.conversationId.get, user)
    //      // cut message, so it will fit in the sms with the footer.
    //      val bodyWithFooter: String = {
    //        if (footer.length + body.length > 160) {
    //          body.substring(0, 160 - footer.length) + footer
    //        } else {
    //          body + footer
    //        }
    //      }
    //
    //      sendSMS(from, to, bodyWithFooter)
    //    }
  }

}
