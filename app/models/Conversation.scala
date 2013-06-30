package models

import java.util.Date
import traits.Model
import play.api.libs.json._
import reactivemongo.api.indexes.{IndexType, Index}
import scala.concurrent.{Future, ExecutionContext}
import ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 1:29 PM
 */

case class Conversation(
                         conversationId: String,
                         created: Date,
                         lastUpdated: Date,
                         recipients: Seq[Recipient],
                         messages: Seq[Message]
                         )

object Conversation extends Model[Conversation] {

  conversationCollection.indexesManager.ensure(Index(List("conversationId" -> IndexType.Ascending), unique = true,
    sparse = true))

  implicit val collection = conversationCollection
  implicit val mongoFormat: Format[Conversation] = createMongoFormat(Json.reads[Conversation],
    Json.writes[Conversation])

  val inputReads = Json.reads[Conversation]

  val outputWrites = Writes[Conversation] {
    conversation =>
      Json.obj("conversationId" -> conversation.conversationId) ++
        Recipient.toSortedJsonArray("recipients", conversation.recipients) ++
        Message.toSortedJsonArray("messages", conversation.messages) ++
        Json.obj("created" -> defaultDateFormat.format(conversation.created)) ++
        addCreated(conversation.created) ++
        addLastUpdated(conversation.lastUpdated)
  }

  val summaryWrites = Writes[Conversation] {
    c =>
      Json.obj("conversationId" -> c.conversationId) ++
        Conversation.addLastUpdated(c.lastUpdated) ++
        Json.obj("numberOfMessages: " -> c.messages.length)
  }

  def find(conversationId: String): Future[Option[Conversation]] = {
    val query = Json.obj("conversationId" -> conversationId)
    collection.find(query).one[Conversation]
  }

}
