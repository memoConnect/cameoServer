package traits

import play.modules.reactivemongo.json.collection.JSONCollection
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 7:10 PM
 */
trait MongoCollections {

  val mongoDB = ReactiveMongoPlugin.db

  lazy val conversationCollection: JSONCollection = mongoDB.collection[JSONCollection]("conversations")
  lazy val userCollection: JSONCollection = mongoDB.collection[JSONCollection]("users")
  lazy val tokenCollection: JSONCollection = mongoDB.collection[JSONCollection]("token")
}
