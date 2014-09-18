package models

import java.util.Date

import helper.{ IdHelper, JsonHelper, MongoCollections }
import play.api.libs.json._
import play.modules.reactivemongo.json.BSONFormats._
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.{ Count, FindAndModify, Update }
import traits.Model

import scala.concurrent.ExecutionContext.Implicits.global
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
                             docVersion: Int) {
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

  def countUserSubscriptions(identityId: MongoId): Future[Int] = {
    val query: BSONDocument = BSONDocument("identityId" -> toBSON(Json.toJson(identityId)).get)
    MongoCollections.mongoDB.command[Int](Count(col.name, Some(query)))
  }

  // store events to all event queues of an identity
  def storeEvent(identityId: MongoId, events: Seq[Event]): Future[Boolean] = {
    val query = Json.obj("identityId" -> identityId)
    val set = Json.obj("$push" -> Json.obj("events" -> Json.obj("$each" -> events)))
    col.update(query, set, multi = true).map { _.ok }
  }
  def storeEvent(identityId: MongoId, event: Event): Future[Boolean] = {
    storeEvent(identityId, Seq(event))
  }

  // get all events for subscriptions and clear them
  def findAndClear(id: MongoId, identityId: MongoId): Future[Option[EventSubscription]] = {
    val query = Json.obj("_id" -> id, "identityId" -> identityId)
    val bsonQuery = BSONDocument("_id" -> toBSON(Json.toJson(id)).get, "identityId" -> toBSON(Json.toJson(identityId)).get)
    val set = Json.obj("$set" -> Json.obj("events" -> JsArray(), "lastAccessed" -> Json.obj("$date" -> new Date)))
    val setBson = JsonHelper.toBson(set).get

    val command = FindAndModify(
      col.name,
      bsonQuery,
      Update(setBson, fetchNewObject = false))

    MongoCollections.mongoDB.command(command).map {
      maybeBson =>
        maybeBson.map {
          bson => Json.toJson(bson).as[EventSubscription]
        }
    }
  }

  def createDefault(): EventSubscription = new EventSubscription(IdHelper.generateEventSubscriptionId(), Seq(), new Date, new MongoId(""), docVersion)

  def docVersion: Int = 0
  def evolutions: Map[Int, Reads[JsObject]] = Map()
}
