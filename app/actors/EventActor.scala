package actors

import akka.actor.Actor
import models._
import play.api.libs.json.{JsObject, Json}
import traits.EventMessage

/**
 * User: Björn Reimer
 * Date: 12.05.14
 * Time: 13:28
 */

case class NewMessage(identityId: MongoId, conversationId: MongoId, message: Message) extends EventMessage {

  def eventType = "conversation:new-message"

  def toEventContent = Json.obj(
    "conversationId" -> conversationId.toJson,
    "message" -> message.toJson
  )
}

case class NewConversation(identityId: MongoId, conversation: Conversation) extends EventMessage {

  def eventType: String = "conversation:new"

  def toEventContent: JsObject = conversation.toJson
}

case class NewFriendRequest(identityId: MongoId, friendRequest: FriendRequest) extends EventMessage {

  def eventType = "friendRequest:new"

  def toEventContent = friendRequest.toJson
}

class EventActor extends Actor {

  def receive() = {
    case msg: EventMessage =>
      EventSubscription.pushEvent(msg.identityId, msg.toEvent)
  }

}
