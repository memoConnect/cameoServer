package traits

import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import play.modules.reactivemongo.json.collection.JSONCollection

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
  val accountCollection: JSONCollection = mongoDB.collection[JSONCollection]("accounts")
  val identityCollection: JSONCollection = mongoDB.collection[JSONCollection]("identities")
  val tokenCollection: JSONCollection = mongoDB.collection[JSONCollection]("token")
  val testCollection: JSONCollection = mongoDB.collection[JSONCollection]("test")
  val purlCollection: JSONCollection = mongoDB.collection[JSONCollection]("purl")

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

  // converst id to _id
  val toMongoId: Reads[JsObject] = {
    __.json.update((__ \ '_id).json.copyFrom((__ \ 'id).json.pick[JsString]) or emptyObj)
  }

  val fromMongoId: Reads[JsObject] = {
    __.json.update((__ \ 'id).json.copyFrom((__ \ '_id).json.pick[JsString]) or emptyObj)
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
}
