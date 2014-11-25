package events

import helper.JsonHelper
import models._
import play.api.libs.json.{ JsNumber, JsObject, Json }

/**
 * User: Björn Reimer
 * Date: 20.11.14
 * Time: 13:57
 */

trait NewMessageEvent extends EventDefinition {

  def conversationId: MongoId

  def unreadMessages: Int

  def message: Message

  def eventType = "conversation:new-message"

  def toEventContent = Json.obj(
    "conversationId" -> conversationId,
    "message" -> message.toJson,
    "unreadMessages" -> unreadMessages
  )

}

case class ConversationNewMessageWithPush(sendToIdentity: MongoId, messageSender: Identity, conversationId: MongoId, unreadMessages: Int, message: Message) extends NewMessageEvent with PushEvent {

  def context = "message:" + conversationId

  def localizationKeyTitle = "PUSH_MESSAGE.NEW_MESSAGE.TITLE"
  def localizationKeyMsg = "PUSH_MESSAGE.NEW_MESSAGE.MSG"

  def localizationVariables = Map {
    "sender" -> messageSender.getDisplayName
  }

}

case class ConversationNewMessage(sendToIdentity: MongoId, conversationId: MongoId, unreadMessages: Int, message: Message) extends NewMessageEvent

case class ConversationNew(sendToIdentity: MongoId, conversation: Conversation) extends EventDefinition {

  def eventType: String = "conversation:new"

  def toEventContent: JsObject = conversation.toJson(sendToIdentity, None) ++ Json.obj("unreadMessages" -> 0)
}

case class ConversationUpdate(sendToIdentity: MongoId, conversationId: MongoId, updatedValues: JsObject) extends EventDefinition {

  def eventType: String = "conversation:update"

  def toEventContent: JsObject = updatedValues ++ Json.obj("id" -> conversationId.toJson)

}