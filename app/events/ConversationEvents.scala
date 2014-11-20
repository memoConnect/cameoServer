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

  def conversation: Conversation

  def message: Message

  def userSettings: Option[AccountUserSettings]

  def eventType = "conversation:new-message"

  def toEventContent = Json.obj(
    "conversationId" -> conversation.id.toJson,
    "message" -> message.toJson,
    "unreadMessages" -> conversation.getNumberOfUnreadMessages(sendToIdentity, userSettings)
  )

}

case class ConversationNewMessageWithPush(sendToIdentity: MongoId, messageSender: Identity, conversation: Conversation, message: Message, userSettings: Option[AccountUserSettings]) extends NewMessageEvent with PushEvent {

  def context = "message:" + conversation.id

  def localizationKeyTitle = "PUSH_MESSAGE.NEW_MESSAGE.TITLE"
  def localizationKeyMsg = "PUSH_MESSAGE.NEW_MESSAGE.MSG"

  def localizationVariables = Map {
    "sender" -> messageSender.getDisplayName
  }

}

case class ConversationNewMessage(sendToIdentity: MongoId, conversation: Conversation, message: Message, userSettings: Option[AccountUserSettings]) extends NewMessageEvent

case class ConversationNew(sendToIdentity: MongoId, conversation: Conversation) extends EventDefinition {

  def eventType: String = "conversation:new"

  def toEventContent: JsObject = conversation.toJson(sendToIdentity, None) ++ Json.obj("unreadMessages" -> 0)
}

case class ConversationUpdate(sendToIdentity: MongoId, conversationId: MongoId, updatedValues: JsObject) extends EventDefinition {

  def eventType: String = "conversation:update"

  def toEventContent: JsObject = updatedValues ++ Json.obj("id" -> conversationId.toJson)

}