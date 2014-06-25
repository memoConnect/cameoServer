package actors

import akka.actor.Actor
import play.api.{ Play, Logger }
import play.api.Play.current
import play.api.libs.json.{ JsValue, Json }
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
class SendSmsActor extends Actor {

  def sendSms(sms: SmsMessage) = {

    val key = Play.configuration.getString("nexmo.key")
    val secret = Play.configuration.getString("nexmo.secret")

    key.isEmpty || secret.isEmpty match {
      case true => {
        Logger.warn("No Nexmo credentials")
        Future(new MessageStatus(new MongoId(""), MESSAGE_STATUS_ERROR, "No Credentials"))
      }
      case false => {

        val postBody =
          Json.obj(
            "api_key" -> JsString(key.get),
            "api_secret" -> JsString(secret.get),
            "from" -> sms.from,
            "to" -> sms.to,
            "text" -> sms.body
          )

        val response = WS.url(Play.configuration.getString("nexmo.url").getOrElse("")).post(postBody)

        response.map {
          nexmoResponse =>
            {
              nexmoResponse.status match {
                case s if s < 300 =>
                  val msg = (nexmoResponse.json \ "messages")(0).asOpt[JsValue].getOrElse(Json.obj())
                  (msg \ "status").asOpt[String] match {
                    case Some("0") =>
                      val id = (msg \ "message-id").asOpt[String].getOrElse("none")
                      val cost = (msg \ "message-price").asOpt[String].getOrElse("none")
                      Logger.info(
                        "SendSMSActor: Sent SMS to " + sms.to +
                          " from " + sms.from +
                          " content: " + sms.body +
                          " NexmoId: " + id +
                          " cost: " + cost)
                    case _ =>
                      Logger.error("SendSMSActor: error sending sms: " + msg)
                  }
                case s =>
                  Logger.error("SendSMSActor: error connection to nexmo: " + nexmoResponse.json)
              }
            }
        }
      }
    }
  }

  def receive = {
    // send message to recipient
    case (message: Message, fromIdentity: Identity, toIdentity: Identity, phoneNumber: String, tryCount: Int) =>
      // check how often we tried to send this message
      if (tryCount > MESSAGE_MAX_TRY_COUNT) {
        val ms = new MessageStatus(toIdentity.id, MESSAGE_STATUS_ERROR, "max try count reached")
        //        message.updateSingleStatus(ms)
      } else {
        // get identity of sender
        val from: String = fromIdentity.displayName.getOrElse(fromIdentity.cameoId)
        val to: String = phoneNumber
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

        // check if we have a test user and save message
        val testUserPrefix = Play.configuration.getString("testUser.prefix").getOrElse("foo")
        toIdentity.cameoId.startsWith(testUserPrefix) match {
          case false =>
          case true  => TestUserNotification.createAndInsert(toIdentity.id, "sms", bodyWithFooter, false)
        }

        // check if this message comes from a test user and save it
        fromIdentity.cameoId.startsWith(testUserPrefix) match {
          case false =>
          case true  => TestUserNotification.createAndInsert(fromIdentity.id, "sms", bodyWithFooter, true)
        }

        sendSms(sms)
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
