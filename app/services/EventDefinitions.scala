package services

import models._
import play.api.libs.json.{JsObject, Json}

/**
 * User: Björn Reimer
 * Date: 02.09.14
 * Time: 15:27
 */

trait EventDefinition {

  def sendToIdentity: MongoId

  def eventType: String

  def toEventContent: JsObject

  def toEvent: Event = {
    Event(
      helper.IdHelper.generateEventId(),
      this.eventType,
      this.toEventContent
    )
  }
}

trait PushEvent {
  def languageKey: String
}

case class NewMessage(sendToIdentity: MongoId, conversationId: MongoId, message: Message) extends EventDefinition with PushEvent {

  def eventType = "conversation:new-message"

  def toEventContent = Json.obj(
    "conversationId" -> conversationId.toJson,
    "message" -> message.toJson
  )

  def languageKey = "NEW_MESSAGE"
}

case class NewConversation(sendToIdentity: MongoId, conversation: Conversation) extends EventDefinition {

  def eventType: String = "conversation:new"

  def toEventContent: JsObject = conversation.toJson
}

case class NewFriendRequest(sendToIdentity: MongoId, friendRequest: FriendRequest, fromIdentity: Identity, toIdentityId: MongoId) extends EventDefinition {

  def eventType = "friendRequest:new"

  def toEventContent =
    Json.obj(
      "friendRequest" -> friendRequest.toJsonWithIdentity(fromIdentity),
      "to" -> toIdentityId.toJson
    )
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

case class NewAePassphrases(sendToIdentity: MongoId, keyId: String, conversationIds: Seq[String]) extends EventDefinition {
  def eventType = "conversation:new-aePassphrase"

  def toEventContent =
    Json.obj(
      "keyId" -> keyId,
      "conversationIds" -> conversationIds
    )
}

case class BroadcastEvent(sendToIdentity: MongoId, eventType: String, content: JsObject) extends EventDefinition {
  def toEventContent: JsObject = content
}