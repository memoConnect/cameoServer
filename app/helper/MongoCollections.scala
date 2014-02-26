package helper

import play.modules.reactivemongo.ReactiveMongoPlugin
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.api.indexes.{ IndexType, Index }
import play.api.Play.current
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global

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
    col.indexesManager.ensure(Index(Seq("recipients.identityId._id" -> IndexType.Ascending)))
    col
  }
  lazy val identityCollection: JSONCollection = {
    val col = mongoDB.collection[JSONCollection]("identities")
    col.indexesManager.ensure(Index(Seq("cameoId" -> IndexType.Ascending)))
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
  lazy val reservedAccountCollection: JSONCollection = mongoDB.collection[JSONCollection]("reservedAccounts")
  lazy val purlCollection: JSONCollection = mongoDB.collection[JSONCollection]("purls")
  lazy val fileChunkCollection: JSONCollection = mongoDB.collection[JSONCollection]("fileChunks")
  lazy val fileMetaCollection: JSONCollection = mongoDB.collection[JSONCollection]("fileMeta")
  lazy val globalStateCollection: JSONCollection = mongoDB.collection[JSONCollection]("globalState")
}
