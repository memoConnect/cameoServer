package models

import java.util.Date
import traits.Model
import play.api.libs.json.{ Json, Format, JsObject, Reads }
import play.modules.reactivemongo.json.collection.JSONCollection
import helper.{ IdHelper, MongoCollections }
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 09.05.14
 * Time: 13:19
 */
case class EventSubscription(id: MongoId,
                             events: Seq[Event],
                             lastAccessed: Date,
                             identityId: MongoId,
                             docVersion: Int)  {
  def toJson: JsObject = Json.obj(
    "events" -> events.map(_.toJson),
    "id" -> id.toJson)
}

object EventSubscription extends Model[EventSubscription] {
  def col: JSONCollection = MongoCollections.eventSubscriptionCollection

  implicit def mongoFormat: Format[EventSubscription] = createMongoFormat(Json.reads[EventSubscription], Json.writes[EventSubscription])

  def create(identityId: MongoId): EventSubscription = {
    new EventSubscription(
      IdHelper.generateEventSubscriptionId(),
      Seq(),
      new Date,
      identityId,
      docVersion
    )
  }

  def createDefault(): EventSubscription = new EventSubscription(IdHelper.generateEventSubscriptionId(), Seq(), new Date,new MongoId(""), docVersion)

  def docVersion: Int = 0
  def evolutions: Map[Int, Reads[JsObject]] = Map()
}
