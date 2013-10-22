package models

import java.util.Date
import traits.{OutputLimits, Model}
import play.api.libs.json._
import reactivemongo.api.indexes.{IndexType, Index}
import scala.concurrent.{Future, ExecutionContext}
import ExecutionContext.Implicits.global
import play.api.Logger

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
                         messages: Seq[Message],
                         lastMessage: Option[Message]
                         )

object Conversation extends Model[Conversation] {

  conversationCollection.indexesManager.ensure(Index(List("conversationId" -> IndexType.Ascending), unique = true,
    sparse = true))

  implicit val collection = conversationCollection
  implicit val mongoFormat: Format[Conversation] = createMongoFormat(Json.reads[Conversation],
    Json.writes[Conversation])

  def inputReads = Json.reads[Conversation]

  def outputWrites(implicit ol: OutputLimits) = Writes[Conversation] {
    conversation =>
      Json.obj("conversationId" -> conversation.conversationId) ++
        Recipient.toSortedJsonObject("recipients", conversation.recipients) ++
        Message.toSortedJsonObject("messages", conversation.messages) ++
        Json.obj("numberOfMessages" -> conversation.messages.length) ++
        Json.obj("created" -> defaultDateFormat.format(conversation.created)) ++
        addCreated(conversation.created) ++
        addLastUpdated(conversation.lastUpdated)
  }

  val summaryWrites = Writes[Conversation] {
    c =>
      Json.obj("conversationId" -> c.conversationId) ++
        Conversation.addLastUpdated(c.lastUpdated) ++
        Json.obj("numberOfMessages" -> c.messages.length) ++
        Json.obj("lastMessage" -> {
          c.lastMessage match {
            case Some(m: Message) => JsString(m.from + ": " + m.messageBody)
            case _ => Json.obj()
          }
        }) ++
        Json.obj("recipients" -> c.recipients.map { _.name })
  }

  def find(conversationId: String): Future[Option[Conversation]] = {
    val query = Json.obj("conversationId" -> conversationId)
    collection.find(query).one[Conversation]
  }

  override val sortWith = {
    (c1: Conversation, c2: Conversation) => c1.lastUpdated.after(c2.lastUpdated)
  }

  def addMessage(message: Message) = {
    val query = Json.obj("conversationId" -> message.conversationId.get)
    val set = Json.obj("$push" -> Json.obj("messages" -> message))
    collection.update(query, set).map {
      lastError => {
        if (lastError.inError) {
          Logger.error("Error adding message to conversation: " + lastError.stringify)
        }
      }
    }
  }

//  def checkAccessRights(conversation: Conversation, user: String): Boolean = {
//    conversation.recipients.exists(r => {
//      Logger.debug("COMPARE: " + user + " | " + Recipient.toJson(r).toString())
//      if (r.messageType.equals("otherUser")) {
//        r.sendTo.equals(user)
//      } else {
//        r.name.equals(user)
//      }
//    })
//  }
}
