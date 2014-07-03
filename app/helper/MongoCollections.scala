package helper

import play.api.Play
import play.api.Play.current
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api.indexes.{ Index, IndexType }
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 2/26/14
 * Time: 12:03 PM
 */
object MongoCollections {

  val mongoDB = ReactiveMongoPlugin.db

  lazy val conversationCollection: JSONCollection = {
    val col = mongoDB.collection[JSONCollection]("conversations")
    col.indexesManager.ensure(Index(Seq("messages._id" -> IndexType.Ascending)))
    col.indexesManager.ensure(Index(Seq("recipients.identityId" -> IndexType.Ascending)))
    col
  }
  lazy val identityCollection: JSONCollection = {
    val col = mongoDB.collection[JSONCollection]("identities")
    col.indexesManager.ensure(Index(Seq("cameoId" -> IndexType.Ascending), unique = true, dropDups = true, sparse = true))
    col.indexesManager.ensure(Index(Seq("contacts._id" -> IndexType.Ascending)))
    col.indexesManager.ensure(Index(Seq("tokens._id" -> IndexType.Ascending)))
    col
  }
  lazy val verificationCollection: JSONCollection = {
    // TODO: create ttl index to expire verification secrets
    val col = mongoDB.collection[JSONCollection]("verifications")
    col
  }
  lazy val accountCollection: JSONCollection = {
    val col = mongoDB.collection[JSONCollection]("accounts")
    col.indexesManager.ensure(Index(List("loginName" -> IndexType.Ascending), unique = true, sparse = true))
    col
  }
  lazy val twoFactorTokenCollection: JSONCollection = {
    val col = mongoDB.collection[JSONCollection]("twoFactorTokens")
    // expire after 1 hour
    val options: BSONDocument = JsonHelper.toBson(Json.obj("expireAfterSeconds" -> (1 * 60 * 60))).get
    col.indexesManager.ensure(Index(List("created" -> IndexType.Ascending), options = options))
    col
  }
  lazy val twoFactorSmsKeyCollection: JSONCollection = {
    val col = mongoDB.collection[JSONCollection]("twoFactorSmsKeys")
    // expire after 10 min
    val options: BSONDocument = JsonHelper.toBson(Json.obj("expireAfterSeconds" -> (10 * 60))).get
    col.indexesManager.ensure(Index(List("created" -> IndexType.Ascending), options = options))
    col
  }
  lazy val eventSubscriptionCollection: JSONCollection = {
    val col = mongoDB.collection[JSONCollection]("eventSubscriptions")
    // expire subscriptions
    val expireAfter = Play.configuration.getInt("events.subscription.expire.period").get
    val options: BSONDocument = JsonHelper.toBson(Json.obj("expireAfterSeconds" -> expireAfter)).get
    col.indexesManager.ensure(Index(List("lastAccessed" -> IndexType.Ascending), options = options))
    col.indexesManager.ensure(Index(List("identityId" -> IndexType.Ascending)))
    col
  }
  lazy val testUserNotificationCollection: JSONCollection = {
    val col = mongoDB.collection[JSONCollection]("testUserNotifications")
    col.indexesManager.ensure(Index(Seq("identityId" -> IndexType.Ascending)))
    col
  }
  lazy val reservedAccountCollection: JSONCollection = {
    val col = mongoDB.collection[JSONCollection]("reservedAccounts")
    // expire reservations
    val expireAfter = Play.configuration.getInt("loginName.reservation.timeout").get * 60
    val options: BSONDocument = JsonHelper.toBson(Json.obj("expireAfterSeconds" -> expireAfter)).get
    col.indexesManager.ensure(Index(List("created" -> IndexType.Ascending), options = options))
    col
  }
  lazy val purlCollection: JSONCollection = mongoDB.collection[JSONCollection]("purls")
  lazy val fileMetaCollection: JSONCollection = mongoDB.collection[JSONCollection]("fileMeta")
  lazy val globalStateCollection: JSONCollection = mongoDB.collection[JSONCollection]("globalState")
  lazy val cockpitAccessCollection: JSONCollection = mongoDB.collection[JSONCollection]("cockpitAccess")

  // chunks are directly saved as BSON
  lazy val fileChunkCollection: BSONCollection = mongoDB.collection[BSONCollection]("fileChunks")

}
