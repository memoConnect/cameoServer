package traits

import play.modules.reactivemongo.json.collection.JSONCollection
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json.{Json, JsObject}
import ExecutionContext.Implicits.global

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
    conversationCollection.find(Json.obj("messages." + messageId -> Json.obj("$exists" -> true)),
      Json.obj("messages." + messageId ->
        true)).one[JsObject]
  }
}
