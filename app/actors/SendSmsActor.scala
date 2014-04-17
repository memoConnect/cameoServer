package actors

import akka.actor.{ Props, Actor }
import play.api.{ Play, Logger }
import play.api.Play.current
import play.api.libs.json.{ JsValue, Json }
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws.WS
import scala.concurrent.Future
import models._
import constants.Messaging._
import play.api.libs.json.JsString
import play.api.libs.concurrent.Akka

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 8:01 PM
 */
class SendSmsActor extends Actor {

  def sendSms(sms: SmsMessage): Future[MessageStatus] = {

    Logger.info("SendSMSActor: To: " + sms.to + " Content: " + sms.body)

    val key = Play.configuration.getString("nexmo.key")
    val secret = Play.configuration.getString("nexmo.secret")

    key.isEmpty || secret.isEmpty match {
      case true => {
        Logger.warn("No Nexmo credentials")
        Future(new MessageStatus(new MongoId(""), MESSAGE_STATUS_ERROR, "No Credentials"))
      }
      case false => {

        val postBody = Json.obj("api_key" -> JsString(key.get), "api_secret" -> JsString(secret.get),
          "from" -> sms.from, "to" -> sms.to, "text" -> sms.body)

        val response = WS.url(Play.configuration.getString("nexmo.url").getOrElse("")).post(postBody)

        response.map {
          nexmoResponse =>
            {
              val messageStatus: MessageStatus = {
                if (nexmoResponse.status < 300) {
                  val jsResponse = nexmoResponse.json
                  val messageReport = (jsResponse \ "messages")(0).asOpt[JsValue].get
                  if ((messageReport \ "status").as[Int] == 0) {
                    val s = "SMS Send. Id: " + (messageReport \ "message-id").asOpt[String].getOrElse("none") + " Network:" + (messageReport \ "network").asOpt[String].getOrElse("none")
                    new MessageStatus(new MongoId(""), MESSAGE_STATUS_SEND, s)
                  } else {
                    val s = "Error sending SMS message. Response: " + jsResponse.toString
                    new MessageStatus(new MongoId(""), MESSAGE_STATUS_ERROR, s)
                  }
                } else {
                  val s = "Error connecting to Nexmo: " + nexmoResponse.statusText
                  new MessageStatus(new MongoId(""), MESSAGE_STATUS_ERROR, s)
                }
              }

              Logger.info("SendSMSActor: Sent SMS to " + sms.to + " from " + sms.from + " STATUS: " + messageStatus)
              messageStatus
            }
        }
      }
    }
  }

  def receive = {
    // send message to recipient
    case (message: Message, fromIdentity: Identity, toIdentity: Identity, tryCount: Int) =>

      // check how often we tried to send this message
      if (tryCount > MESSAGE_MAX_TRY_COUNT) {
        val ms = new MessageStatus(toIdentity.id, MESSAGE_STATUS_ERROR, "max try count reached")
        message.updateSingleStatus(ms)
      } else {
        // get identity of sender
        val from: String = fromIdentity.displayName.getOrElse(fromIdentity.cameoId)
        val to: String = toIdentity.phoneNumber.get.toString
        val body: String = message.plain match {
          case Some(PlainMessagePart(Some(text), _)) => text
          case _                                     => MESSAGE_TEXT_REPLACE_ENCRYPTED
        }

        // create purl 
        val purl = Purl.create(message.id, toIdentity.id)
        Purl.col.insert(purl)

        // add footer to sms
        val footer = "... more: " + Play.configuration.getString("shortUrl.address").getOrElse("none") + "/p/" + purl.id

        // cut message, so it will fit in the sms with the footer.
        val bodyWithFooter: String = {
          if (footer.length + body.length > 160) {
            body.substring(0, 160 - footer.length) + footer
          } else {
            body + footer
          }
        }

        val sms = new SmsMessage(from, to, bodyWithFooter)

        sendSms(sms).map {
          messageStatus =>
            {
              if (messageStatus.status.equals(MESSAGE_STATUS_SEND)) {
                message.updateSingleStatus(messageStatus.copy(identityId = toIdentity.id))
              } else {
                // try again
                lazy val sendSmsActor = Akka.system.actorOf(Props[SendSmsActor])
                sendSmsActor ! (message, fromIdentity, toIdentity, tryCount + 1)
              }
            }
        }
      }

    case (sms: SmsMessage, tryCount: Int) => {
      if (tryCount > MESSAGE_MAX_TRY_COUNT) {
        Logger.error("Max try count of message reached: " + sms)
      } else {
        sendSms(sms)
      }
    }
  }

}
