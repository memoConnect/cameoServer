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

case class NewMessage(sendToIdentity: MongoId, conversationId: MongoId, message: Message) extends EventMessage {

  def eventType = "conversation:new-message"

  def toEventContent = Json.obj(
    "conversationId" -> conversationId.toJson,
    "message" -> message.toJson
  )
}

case class NewConversation(sendToIdentity: MongoId, conversation: Conversation) extends EventMessage {

  def eventType: String = "conversation:new"

  def toEventContent: JsObject = conversation.toJson
}

case class NewFriendRequest(sendToIdentity: MongoId, friendRequest: FriendRequest, fromIdentity: Identity, toIdentityId: MongoId) extends EventMessage {

  def eventType = "friendRequest:new"

  def toEventContent =
    Json.obj(
      "friendRequest" -> friendRequest.toJsonWithIdentity(fromIdentity),
      "to" -> toIdentityId.toJson
    )
}

case class AcceptedFriendRequest(sendToIdentity: MongoId, fromIdentity: MongoId, toIdentityId: MongoId) extends EventMessage {

  def eventType = "friendRequest:accepted"

  def toEventContent =
    Json.obj(
      "from" -> fromIdentity.toJson,
      "to" -> toIdentityId.toJson
    )

}

class EventActor extends Actor {

  def receive() = {
    case msg: EventMessage =>
      EventSubscription.pushEvent(msg.sendToIdentity, msg.toEvent)
  }

}
