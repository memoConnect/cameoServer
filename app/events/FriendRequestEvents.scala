package events

import models.{ FriendRequest, Identity, MongoId }
import play.api.libs.json.{ JsObject, Json }

/**
 * User: Björn Reimer
 * Date: 20.11.14
 * Time: 13:59
 */

case class FriendRequestNew(sendToIdentity: MongoId, friendRequest: FriendRequest, fromIdentity: Identity, toIdentityId: MongoId) extends EventDefinition with PushEvent {

  def eventType = "friendRequest:new"

  def toEventContent =
    Json.obj(
      "friendRequest" -> friendRequest.toJsonWithIdentity(fromIdentity),
      "to" -> toIdentityId.toJson
    )

  def context = "friendRequest"

  def localizationKeyTitle = "PUSH_MESSAGE.FRIEND_REQUEST.TITLE"
  def localizationKeyMsg: String = "PUSH_MESSAGE.FRIEND_REQUEST.MSG"

  def localizationVariables = Map {
    "sender" -> fromIdentity.getDisplayName
  }
}

case class FriendRequestAccepted(sendToIdentity: MongoId, fromIdentity: MongoId, toIdentityId: MongoId, contact: JsObject) extends EventDefinition {

  def eventType = "friendRequest:accepted"

  def toEventContent =
    Json.obj(
      "from" -> fromIdentity.toJson,
      "to" -> toIdentityId.toJson,
      "contact" -> contact
    )
}

case class FriendRequestRejected(sendToIdentity: MongoId, fromIdentity: MongoId, toIdentityId: MongoId) extends EventDefinition {

  def eventType = "friendRequest:rejected"

  def toEventContent =
    Json.obj(
      "from" -> fromIdentity.toJson,
      "to" -> toIdentityId.toJson
    )
}