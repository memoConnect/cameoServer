package actors

import akka.actor.Actor
import models._
import play.api.Logger
import play.api.libs.json.{Writes, Json, JsObject}
import play.api.libs.json.JsObject
import traits.EventMessage

/**
 * User: Björn Reimer
 * Date: 12.05.14
 * Time: 13:28
 */

case class NewMessage(identityId: MongoId, conversationId: MongoId, message: Message) extends EventMessage {

  def eventType = "conversation:new-message"

  def toEventJson: JsObject =
    Json.obj(
      "conversationId" -> conversationId.toJson,
      "message" -> message.toJson
    )
}

class EventActor extends Actor {

  def receive() = {
    case msg: NewMessage =>
      EventSubscription.pushEvent(msg.identityId, msg.toEvent)
  }

}
