package models

import java.util.Date
import traits.{OutputLimits, Model, MongoHelper}
import play.api.libs.json._
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global
import reactivemongo.api.indexes.{IndexType, Index}

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 9:31 PM
 */
case class Token(
                  token: String,
                  username: String,
                  isAdmin: Boolean,
                  created: Date
                  )

object Token extends MongoHelper with Model[Token] {

  tokenCollection.indexesManager.ensure(Index(List("token" -> IndexType.Ascending), unique = true, sparse = true))

  implicit val collection = tokenCollection
  implicit val mongoFormat: Format[Token] = createMongoFormat(Json.reads[Token], Json.writes[Token])

  def inputReads = Json.reads[Token]

  def outputWrites(implicit ol: OutputLimits = OutputLimits(0,0)) = Writes[Token] {
    t =>
      Json.obj("token" -> t.token) ++
        Json.obj("username" -> t.username) ++
        addCreated(t.created)
  }

  def find(token: String): Future[Option[Token]] = {
    val query = Json.obj("token" -> token)
    collection.find(query).one[Token]
  }
}
