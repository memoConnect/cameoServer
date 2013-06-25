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
trait MongoHelper extends JsonTransformer {

  lazy val mongoDB = ReactiveMongoPlugin.db

  lazy val conversationCollection: JSONCollection = mongoDB.collection[JSONCollection]("conversations")
  lazy val userCollection: JSONCollection = mongoDB.collection[JSONCollection]("users")
  lazy val tokenCollection: JSONCollection = mongoDB.collection[JSONCollection]("token")
  lazy val testCollection: JSONCollection = mongoDB.collection[JSONCollection]("test")

  // converts dates and ids to mongo format ($date and $oid)
  val toMongoDates: Reads[JsObject] = {
    __.json.update((__ \ 'created \ '$date).json.copyFrom((__ \ 'created).json.pick[JsNumber]) or emptyObj) andThen
      __.json.update((__ \ 'lastUpdated \ '$date).json.copyFrom((__ \ 'lastUpdated).json.pick[JsNumber]) or emptyObj)
  }

  val fromMongoDates: Reads[JsObject] = {
    __.json.update((__ \ 'created).json.copyFrom((__ \ 'created \ '$date).json.pick[JsNumber]) or emptyObj) andThen
      __.json.update((__ \ 'lastUpdated).json.copyFrom((__ \ 'lastUpdated \ '$date).json.pick[JsNumber]) or emptyObj)
  }

  // get Array from Document
  def getArray[T](collection: JSONCollection, queryKey: String, queryValue: String, arrayKey: String, reads: Reads[T]): Future[Option[Seq[T]]] = {
    val query = Json.obj(queryKey -> queryValue)
    val filter = Json.obj(arrayKey -> 1)

    collection.find(query, filter).one[JsObject].map {
      case None => None
      case Some(js: JsObject) => Some( (js \ arrayKey).asOpt[Seq[T]](Reads.seq[T](reads)).getOrElse(Seq()))
    }
  }

  // find message in conversation
  def findMessage(messageId: String): Future[Option[JsObject]] = {
    conversationCollection.find(Json.obj("messages." + messageId -> Json.obj("$exists" -> true)), Json.obj("messages." + messageId ->
      true)).one[JsObject].map {
      case Some(m: JsObject) =>
        val res: JsObject = m.transform(__.json.copyFrom((__ \ 'messages \ messageId).json.pick[JsObject])).getOrElse(Json.obj())
        Some(res)
      case None => None
    }
  }
}
