package services

import models._
import play.api.libs.json.{ JsObject, Json }

/**
 * User: Björn Reimer
 * Date: 02.09.14
 * Time: 15:27
 */

trait EventDefinition {

  def sendToIdentity: MongoId

  def eventType: String

  def toEventContent: JsObject

  def fromIdentityId: Option[MongoId] = None

  def toEvent: Event = {
    Event(
      helper.IdHelper.generateEventId(),
      this.eventType,
      this.toEventContent,
      fromIdentityId
    )
  }
}

trait PushEvent {

  def localizationKeyTitle: String
  def localizationKeyMsg: String

  def localizationVariables: Map[String, String]
}

case class NewMessageWithPush(sendToIdentity: MongoId, messageSender: Identity, conversationId: MongoId, message: Message) extends EventDefinition with PushEvent {

  def eventType = "conversation:new-message"

  def toEventContent = Json.obj(
    "conversationId" -> conversationId.toJson,
    "message" -> message.toJson
  )

  def localizationKeyTitle = "PUSH_MESSAGE.NEW_MESSAGE.TITLE"
  def localizationKeyMsg = "PUSH_MESSAGE.NEW_MESSAGE.MSG"

  def localizationVariables = Map {
    "sender" -> messageSender.getDisplayName
  }
}

case class NewMessage(sendToIdentity: MongoId, conversationId: MongoId, message: Message) extends EventDefinition {

  def eventType = "conversation:new-message"

  def toEventContent = Json.obj(
    "conversationId" -> conversationId.toJson,
    "message" -> message.toJson
  )
}

case class NewConversation(sendToIdentity: MongoId, conversation: Conversation) extends EventDefinition {

  def eventType: String = "conversation:new"

  def toEventContent: JsObject = conversation.toJson
}

case class NewFriendRequest(sendToIdentity: MongoId, friendRequest: FriendRequest, fromIdentity: Identity, toIdentityId: MongoId) extends EventDefinition with PushEvent {

  def eventType = "friendRequest:new"

  def toEventContent =
    Json.obj(
      "friendRequest" -> friendRequest.toJsonWithIdentity(fromIdentity),
      "to" -> toIdentityId.toJson
    )

  def localizationKeyTitle = "PUSH_MESSAGE.FRIEND_REQUEST.TITLE"
  def localizationKeyMsg: String = "PUSH_MESSAGE.FRIEND_REQUEST.MSG"

  def localizationVariables = Map {
    "sender" -> fromIdentity.getDisplayName
  }
}

case class AcceptedFriendRequest(sendToIdentity: MongoId, fromIdentity: MongoId, toIdentityId: MongoId, contact: JsObject) extends EventDefinition {

  def eventType = "friendRequest:accepted"

  def toEventContent =
    Json.obj(
      "from" -> fromIdentity.toJson,
      "to" -> toIdentityId.toJson,
      "contact" -> contact
    )
}

case class UpdatedIdentity(sendToIdentity: MongoId, identityId: MongoId, updatedValues: JsObject) extends EventDefinition {

  def eventType = "identity:update"

  def toEventContent = updatedValues ++ Json.obj("id" -> identityId.toJson)

}

case class NewIdentity(sendToIdentity: MongoId, identity: Identity) extends EventDefinition {

  def eventType = "identity:new"

  def toEventContent = identity.toPrivateJson
}

case class BroadcastEvent(sendToIdentity: MongoId, eventType: String, content: JsObject, fromIdentity: Identity) extends EventDefinition {

  override def fromIdentityId = Some(fromIdentity.id)
  
  def toEventContent: JsObject = content
}