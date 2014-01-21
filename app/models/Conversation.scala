package models

import java.util.Date
import traits.Model
import play.api.libs.json._
import scala.concurrent.{Future, ExecutionContext}
import ExecutionContext.Implicits.global
import play.api.Logger
import play.modules.reactivemongo.json.collection.JSONCollection
import helper.{OutputLimits, IdHelper}

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 1:29 PM
 */

case class Conversation(
                         id: MongoId,
                         subject: String,
                         recipients: Seq[MongoId],
                         messages: Seq[Message],
                         lastMessage: Option[Message],
                         created: Date,
                         lastUpdated: Date
                         ) {

  def toJson(offset: Int = 0, limit: Int = 0): JsValue = Json.toJson(this)(Conversation.outputWrites(offset, limit))

}

object Conversation extends Model[Conversation] {

  val col = mongoDB.collection[JSONCollection]("conversations")

  implicit val mongoFormat: Format[Conversation] = createMongoFormat(Json.reads[Conversation], Json.writes[Conversation])

//  def createReads

  def outputWrites(offset: Int, limit: Int) = Writes[Conversation] {
    c =>
      Json.obj("id" -> c.id.toJson) ++
        Json.obj("recipients" -> c.recipients.map(_.toJson)) ++
        Json.obj("messages" -> OutputLimits.applyLimits(c.messages.map(_.toJson), offset, limit)) ++
        Json.obj("numberOfMessages" -> c.messages.length) ++
        addCreated(c.created) ++
        addLastUpdated(c.lastUpdated)
  }

  //  val summaryWrites = Writes[Conversation] {
  //    c =>
  //      Json.obj("conversationId" -> c.id.toJson) ++
  //        Conversation.addLastUpdated(c.lastUpdated) ++
  //        Json.obj("numberOfMessages" -> c.messages.length) ++
  //        Json.obj("lastMessage" -> {
  //          c.lastMessage match {
  //            case Some(m: Message) => JsString(m.from + ": " + m.messageBody)
  //            case _ => Json.obj()
  //          }
  //        }) ++
  //        Json.obj("recipients" -> c.recipients.map {
  //          Identity.getDisplayName
  //        })}

  def find(id: MongoId): Future[Option[Conversation]] = {
    val query = Json.obj("_id" -> id)
    col.find(query).one[Conversation]
  }

  def create: MongoId = {
    val id = new MongoId(IdHelper.generateConversationId())
    new Conversation(id, new Date, new Date, Seq(), Seq(), None)
    id
  }


  def addMessage(message: Message) = {
    val query = Json.obj("conversationId" -> message.conversationId.get)
    val set = Json.obj("$push" -> Json.obj("messages" -> message))
    col.update(query, set).map {
      lastError => {
        if (lastError.inError) {
          Logger.error("Error adding message to conversation: " + lastError.stringify)
        }
      }
    }
  }

  //  def getFromList(ids: Seq[String]): Future[List[Conversation]] = {
  //    val query = Json.obj("$or" -> ids.map(s => Json.obj("conversationId" -> s)))
  //    conversationCollection.find(query).sort(Json.obj("lastUpdated" -> -1)).cursor[Conversation].collect[List]()
  //  }

  //  def hasMember(conversation: Conversation, user: String): Boolean = {
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
