package models

import java.util.Date
import traits.Model
import play.api.libs.json._
import helper.{JsonHelper, IdHelper, MongoCollections}
import scala.concurrent.{ ExecutionContext, Future }
import reactivemongo.core.commands.{ Update, FindAndModify, Count }
import reactivemongo.bson.{BSONDateTime, BSONArray, BSONDocument}
import play.modules.reactivemongo.json.BSONFormats._
import ExecutionContext.Implicits.global
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.json.JsObject
import scala.Some
import scala.util.Success

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

  // push events to all event queues of an identity
  def pushEvent(identityId: MongoId, events: Seq[Event]): Future[Boolean] = {
    val query = Json.obj("identityId" -> identityId)
    val set = Json.obj("$push" -> Json.obj("events" -> Json.obj("$each" -> events)))
    col.update(query, set, multi = true).map { _.ok }
  }
  def pushEvent(identityId: MongoId, event: Event): Future[Boolean] = {
    pushEvent(identityId, Seq(event))
  }

  // get all events for subscriptions and clear them
  def findAndClear(id: MongoId): Future[Option[EventSubscription]] = {
    val query = BSONDocument("_id" -> toBSON(Json.toJson(id)).get)
    val set = Json.obj("$set" -> Json.obj("events" -> JsArray(), "lastAccessed" -> Json.obj("$date" -> new Date)))
    val setBson = JsonHelper.toBson(set).get

    val command = FindAndModify(
      col.name,
      query,
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
