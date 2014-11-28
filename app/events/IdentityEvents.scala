package events

import models.{Signature, Contact, Identity, MongoId}
import play.api.libs.json.{Json, JsObject}

/**
 * User: Björn Reimer
 * Date: 20.11.14
 * Time: 13:58
 */

case class IdentityUpdate(sendToIdentity: MongoId, identityId: MongoId, updatedValues: JsObject) extends EventDefinition {

  def eventType = "identity:update"

  def toEventContent = updatedValues ++ Json.obj("id" -> identityId.toJson)

}

case class IdentityNew(sendToIdentity: MongoId, identity: Identity) extends EventDefinition {

  def eventType = "identity:new"

  def toEventContent = identity.toPrivateJson
}

case class ContactUpdate(sendToIdentity: MongoId, contact: Contact, identity: Identity) extends EventDefinition {

  def eventType = "contact:update"

  def toEventContent = contact.toJsonWithIdentity(None, Seq(identity))

}