package events

import models._
import play.api.libs.json.JsObject

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

trait PushEvent extends EventDefinition {

  def localizationKeyTitle: String

  def localizationKeyMsg: String

  def localizationVariables: Map[String, String]

  def context: String
}

case class BroadcastEvent(sendToIdentity: MongoId, eventType: String, content: JsObject, fromIdentity: Identity) extends EventDefinition {

  override def fromIdentityId = Some(fromIdentity.id)

  def toEventContent: JsObject = content
}
