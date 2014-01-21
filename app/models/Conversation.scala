package models

import java.util.Date
import traits.Model
import scala.concurrent.{ExecutionContext, Future}
import helper.{OutputLimits, IdHelper}
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.modules.reactivemongo.json.collection.JSONCollection
import ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 1:29 PM
 */

case class Conversation(
                         id: MongoId,
                         subject: Option[String],
                         recipients: Seq[MongoId],
                         messages: Seq[Message],
                         created: Date,
                         lastUpdated: Date
                         ) {

  def toJson(offset: Int = 0, limit: Int = 0): JsObject = Json.toJson(this)(Conversation.outputWrites(offset, limit)).as[JsObject]

  def toJson: JsObject = toJson(0, 0)

}

object Conversation extends Model[Conversation] {

  lazy val col: JSONCollection = mongoDB.collection[JSONCollection]("conversations")

  implicit val mongoFormat: Format[Conversation] = createMongoFormat(Json.reads[Conversation], Json.writes[Conversation])

  def createReads = (
    Reads.pure[MongoId](IdHelper.generateConversationId()) and
      (__ \ 'subject).readNullable[String] and
      ((__ \ 'recipients).read[Seq[MongoId]] or Reads.pure[Seq[MongoId]](Seq())) and
      Reads.pure[Seq[Message]](Seq()) and
      Reads.pure[Date](new Date) and
      Reads.pure[Date](new Date)
    )(Conversation.apply _)

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

  def create: Conversation = {
    val id = IdHelper.generateConversationId()
    new Conversation(id, None, Seq(), Seq(), new Date, new Date)
  }


  //  def addMessage(message: Message) = {
  //    val query = Json.obj("conversationId" -> message.conversationId.get)
  //    val set = Json.obj("$push" -> Json.obj("messages" -> message))
  //    col.update(query, set).map {
  //      lastError => {
  //        if (lastError.inError) {
  //          Logger.error("Error adding message to conversation: " + lastError.stringify)
  //        }
  //      }
  //    }
  //  }

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
