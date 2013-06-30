package traits

import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global
import play.modules.reactivemongo.json.collection.JSONCollection
import scala.Some

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 7:10 PM
 */
trait MongoHelper {

  val mongoDB = ReactiveMongoPlugin.db

  val conversationCollection: JSONCollection = mongoDB.collection[JSONCollection]("conversations")
  val userCollection: JSONCollection = mongoDB.collection[JSONCollection]("users")
  val tokenCollection: JSONCollection = mongoDB.collection[JSONCollection]("token")
  val testCollection: JSONCollection = mongoDB.collection[JSONCollection]("test")

  val emptyObj = __.json.put(Json.obj())

  // converts dates and ids to mongo format ($date and $oid)
  val toMongoDates: Reads[JsObject] = {
    __.json.update((__ \ 'created \ '$date).json.copyFrom((__ \ 'created).json.pick[JsNumber]) or emptyObj) andThen
      __.json.update((__ \ 'lastUpdated \ '$date).json.copyFrom((__ \ 'lastUpdated).json.pick[JsNumber]) or emptyObj)
  }

  val fromMongoDates: Reads[JsObject] = {
    __.json.update((__ \ 'created).json.copyFrom((__ \ 'created \ '$date).json.pick[JsNumber]) or emptyObj) andThen
      __.json.update((__ \ 'lastUpdated).json.copyFrom((__ \ 'lastUpdated \ '$date).json.pick[JsNumber]) or emptyObj)
  }

  def createMongoReads[T](reads: Reads[T]): Reads[T] = Reads {
    js => js.transform(fromMongoDates).map {
      contact: JsValue => contact.as[T](reads)
    }
  }

  def createMongoWrites[T](writes: Writes[T]): Writes[T] = Writes {
        obj: T => Json.toJson[T](obj)(writes).transform(toMongoDates).getOrElse(Json.obj())
  }

  def createMongoFormat[T](reads: Reads[T], writes: Writes[T]) = Format(createMongoReads(reads), createMongoWrites(writes))
}
