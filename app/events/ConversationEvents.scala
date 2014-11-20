package events

import helper.JsonHelper
import models.{Conversation, Identity, Message, MongoId}
import play.api.libs.json.{JsObject, Json}

/**
 * User: Björn Reimer
 * Date: 20.11.14
 * Time: 13:57
 */

trait NewMessageEvent extends EventDefinition {

  def conversationId: MongoId

  def message: Message

  def unreadMessages: Option[Int]

  def eventType = "conversation:new-message"

  def toEventContent = Json.obj(
    "conversationId" -> conversationId.toJson,
    "message" -> message.toJson
  )

}

case class ConversationNewMessageWithPush(sendToIdentity: MongoId, messageSender: Identity, conversationId: MongoId, message: Message, unreadMessages: Option[Int]) extends NewMessageEvent with PushEvent {

  def context = "message:" + conversationId.id

  def localizationKeyTitle = "PUSH_MESSAGE.NEW_MESSAGE.TITLE"
  def localizationKeyMsg = "PUSH_MESSAGE.NEW_MESSAGE.MSG"

  def localizationVariables = Map {
    "sender" -> messageSender.getDisplayName
  }

}

case class ConversationNewMessage(sendToIdentity: MongoId, conversationId: MongoId, message: Message, unreadMessages: Option[Int]) extends NewMessageEvent

case class ConversationNew(sendToIdentity: MongoId, conversation: Conversation) extends EventDefinition {

  def eventType: String = "conversation:new"

  def toEventContent: JsObject = conversation.toJson(sendToIdentity)
}

case class ConversationUpdate(sendToIdentity: MongoId, conversationId: MongoId, updatedValues: JsObject) extends EventDefinition {

  def eventType: String = "conversation:update"

  def toEventContent: JsObject = updatedValues ++ Json.obj("id" -> conversationId.toJson)

}