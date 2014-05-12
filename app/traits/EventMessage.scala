package traits

import play.api.libs.json.JsObject
import models.Event

/**
 * User: Björn Reimer
 * Date: 12.05.14
 * Time: 16:23
 */
trait EventMessage {

  def eventType: String

  def toEventJson: JsObject

  def toEvent: Event = {
    Event(
      helper.IdHelper.generateEventId(),
      this.eventType,
      this.toEventJson
    )
  }

}
