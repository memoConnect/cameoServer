package actors

import akka.actor.Actor
import models.{MongoId, Recipient, Message}
import play.api.Logger
import play.api.libs.json.{Writes, Json, JsObject}

/**
 * User: Björn Reimer
 * Date: 12.05.14
 * Time: 13:28
 */

case class NewMessage(conversationId: MongoId, recipients: Seq[Recipient], message: Message) {
  def toEventJson: JsObject =
    Json.obj(
      "conversationId" -> conversationId,
      "message" -> message.toJson
    )
}

class EventActor extends Actor {

  def receive() = {
    case msg: NewMessage =>
      Logger.info("new message!")
  }

}
