package models

import play.api.libs.json.{ Format, JsObject, Json, Reads }
import traits.SubModel

/**
 * User: Björn Reimer
 * Date: 09.05.14
 * Time: 13:15
 */
case class Event(id: MongoId,
                 eventName: String,
                 data: JsObject) {

  def toJson: JsObject =
    Json.obj(
      "name" -> this.eventName,
      "data" -> this.data)

}

object Event extends SubModel[Event, EventSubscription] {

  def parentModel = EventSubscription

  def elementName: String = "events"

  implicit def mongoFormat: Format[Event] = createMongoFormat(Json.reads[Event], Json.writes[Event])

  def createDefault(): Event = new Event(new MongoId(""), "foo", Json.obj())
  def docVersion: Int = 0
  def evolutions: Map[Int, Reads[JsObject]] = Map()
}
