package traits

import play.modules.reactivemongo.json.collection.JSONCollection
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json._
import ExecutionContext.Implicits.global
import play.modules.reactivemongo.json.collection.JSONCollection
import scala.Some
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.json.JsObject
import scala.Some

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 7:10 PM
 */
trait MongoHelper {

  val mongoDB = ReactiveMongoPlugin.db

  lazy val conversationCollection: JSONCollection = mongoDB.collection[JSONCollection]("conversations")
  lazy val userCollection: JSONCollection = mongoDB.collection[JSONCollection]("users")
  lazy val tokenCollection: JSONCollection = mongoDB.collection[JSONCollection]("token")

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
