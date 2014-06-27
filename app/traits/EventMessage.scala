package traits

import models.{ Event, MongoId }
import play.api.libs.json.JsObject

/**
 * User: Björn Reimer
 * Date: 12.05.14
 * Time: 16:23
 */
trait EventMessage {

  def identityId: MongoId

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
