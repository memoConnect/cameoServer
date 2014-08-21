package actors

import akka.actor.Actor
import constants.Messaging._
import models._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.{ JsString, JsValue, Json }
import play.api.libs.ws.WS
import play.api.{ Logger, Play }

import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 8:01 PM
 */
case class Sms(from: String, to: String, body: String)
object Sms { implicit val format = Json.format[Sms] }

class SendSmsActor extends Actor {

  def removeSpecialCharacters(value: String): String = {
    val allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

    val converted: List[String] = value.toList.map {
      case c if allowed.contains(c) =>
      case 'ä'                      => "ae"
      case 'ö'                      => "oe"
      case 'ü'                      => "ue"
      case 'ß'                      => "ss"
      case _                        => " "
    }
    converted.mkString
  }

  def receive = {
    case sms: Sms =>

      val key = Play.configuration.getString("nexmo.key")
      val secret = Play.configuration.getString("nexmo.secret")

      key.isEmpty || secret.isEmpty match {
        case true =>
          Logger.warn("No Nexmo credentials")
        case false =>
          val postBody =
            Json.obj(
              "api_key" -> JsString(key.get),
              "api_secret" -> JsString(secret.get),
              "from" -> removeSpecialCharacters(sms.from),
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
