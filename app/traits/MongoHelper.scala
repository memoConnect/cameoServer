package traits

import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import play.modules.reactivemongo.json.collection.JSONCollection

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import java.text.SimpleDateFormat
import java.util.{Date, TimeZone}
import reactivemongo.api.indexes.{IndexType, Index}

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 7:10 PM
 */
trait MongoHelper {

  val mongoDB = ReactiveMongoPlugin.db

  lazy val userCollection: JSONCollection = mongoDB.collection[JSONCollection]("users")
  lazy val accountCollection: JSONCollection = mongoDB.collection[JSONCollection]("accounts")
  lazy val identityCollection: JSONCollection = mongoDB.collection[JSONCollection]("identities")
  lazy val purlCollection: JSONCollection = mongoDB.collection[JSONCollection]("purl")
  lazy val verificationCollection: JSONCollection = {
    // TODO: create ttl index to expire verification secrets
    val col = mongoDB.collection[JSONCollection]("verifications")
    col
  }

  val emptyObj = __.json.put(Json.obj())

  // converts dates to mongo format ($date)
  val toMongoDates: Reads[JsObject] = {
    __.json.update((__ \ 'created \ '$date).json.copyFrom((__ \ 'created).json.pick[JsNumber]) or emptyObj) andThen
      __.json.update((__ \ 'lastUpdated \ '$date).json.copyFrom((__ \ 'lastUpdated).json.pick[JsNumber]) or emptyObj)
  }

  val fromMongoDates: Reads[JsObject] = {
    __.json.update((__ \ 'created).json.copyFrom((__ \ 'created \ '$date).json.pick[JsNumber]) or emptyObj) andThen
      __.json.update((__ \ 'lastUpdated).json.copyFrom((__ \ 'lastUpdated \ '$date).json.pick[JsNumber]) or emptyObj)
  }

  // converts id to _id
  val toMongoId: Reads[JsObject] = {
    __.json.update((__ \ '_id).json.copyFrom((__ \ 'id).json.pick[JsValue]) or emptyObj) andThen
      (__ \ 'id).json.prune
  }

  val fromMongoId: Reads[JsObject] = {
    __.json.update((__ \ 'id).json.copyFrom((__ \ '_id).json.pick[JsValue]) or emptyObj) andThen
      (__ \ '_id).json.prune
  }

  def createMongoReads[T](reads: Reads[T]): Reads[T] = Reads {
    js => js.transform(fromMongoDates andThen fromMongoId).map {
      obj: JsValue => obj.as[T](reads)
    }
  }

  def createMongoWrites[T](writes: Writes[T]): Writes[T] = Writes {
    obj: T => Json.toJson[T](obj)(writes).transform(toMongoDates andThen toMongoId).getOrElse(Json.obj())
  }

  def createMongoFormat[T](
                            reads: Reads[T],
                            writes: Writes[T]
                            ) = Format(createMongoReads(reads), createMongoWrites(writes))

  val defaultDateFormat: SimpleDateFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss")
  defaultDateFormat.setTimeZone(TimeZone.getTimeZone("Europe/Berlin"))

  def addCreated(date: Date): JsObject = {
    Json.obj("created" -> defaultDateFormat.format(date))
  }

  def addLastUpdated(date: Date): JsObject = {
    Json.obj("lastUpdated" -> defaultDateFormat.format(date))
  }
}
