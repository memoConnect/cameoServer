package models

import java.util.Date
import traits.Model
import scala.concurrent.{ ExecutionContext, Future }
import helper.{ OutputLimits, IdHelper }
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.modules.reactivemongo.json.collection.JSONCollection
import ExecutionContext.Implicits.global
import reactivemongo.core.commands.LastError
import play.api.mvc.SimpleResult
import helper.ResultHelper._
import helper.JsonHelper._
import helper.MongoCollections._
import play.api.Logger

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 1:29 PM
 */

case class Conversation(id: MongoId,
                        subject: Option[String],
                        recipients: Seq[MongoId],
                        messages: Seq[Message],
                        created: Date,
                        lastUpdated: Date) {

  def toJson(offset: Int = 0, limit: Int = 0): JsObject = Json.toJson(this)(Conversation.outputWrites(offset, limit)).as[JsObject]

  def toJson: JsObject = toJson(0, 0)

  def toSummaryJson: JsObject = Json.toJson(this)(Conversation.summaryWrites).as[JsObject]

  def toJsonWithDisplayNames(offset: Int = 0, limit: Int = 0): Future[JsObject] = {
    // get identity of each recipient
    val recipients: Seq[Future[JsObject]] = this.recipients.map {
      id =>
        Identity.find(id).map {
          case None    => Json.obj()
          case Some(i) => i.toSummaryJson
        }
    }

    Future.sequence(recipients).map {
      r =>
        {
          this.toJson(offset, limit) ++ Json.obj("recipients" -> r)
        }
    }
  }

  def toJsonWithDisplayNamesResult(offset: Int = 0, limit: Int = 0): Future[SimpleResult] = {
    this.toJsonWithDisplayNames(offset, limit).map {
      js => resOK(js)
    }
  }

  def addRecipients(recipients: Seq[MongoId]): Future[LastError] = {
    val query = Json.obj("_id" -> this.id)
    val set = Json.obj("$push" ->
      Json.obj("recipients" ->
        Json.obj("$each" -> recipients)))
    Conversation.col.update(query, set)
  }

  def deleteRecipient(recipient: MongoId): Future[Boolean] = {
    val query = Json.obj("_id" -> this.id)
    val set = Json.obj("$pull" ->
      Json.obj("recipients" -> recipient))
    Conversation.col.update(query, set).map { _.updatedExisting }
  }

  def hasMemberFuture(recipient: MongoId)(action: Future[SimpleResult]): Future[SimpleResult] = {
    if (this.recipients.contains(recipient)) {
      action
    } else {
      Future(resUnauthorized("identity is not a member of the conversation"))
    }
  }

  def hasMember(recipient: MongoId)(action: SimpleResult): SimpleResult = {
    if (this.recipients.contains(recipient)) {
      action
    } else {
      resUnauthorized("identity is not a member of the conversation")
    }
  }

  def addMessage(message: Message): Future[LastError] = {
    val query = Json.obj("_id" -> this.id)
    val set = Json.obj("$push" ->
      Json.obj("messages" -> Json.obj(
        "$each" -> Seq(message),
        "$sort" -> Json.obj("created" -> 1),
        "$slice" -> (-1) * (this.messages.length + 2))))

    Conversation.col.update(query, set)
    //TODO: update lastUpdated
  }

  def getMessage(messageId: MongoId): Option[Message] = {
    this.messages.find(_.id.equals(messageId))
  }
}

object Conversation extends Model[Conversation] {

  def col: JSONCollection = conversationCollection

  implicit val mongoFormat: Format[Conversation] = createMongoFormat(Json.reads[Conversation], Json.writes[Conversation])

  def docVersion = 0

  def evolutions = Map()

  def createReads = (
    Reads.pure[MongoId](IdHelper.generateConversationId()) and
    (__ \ 'subject).readNullable[String] and
    ((__ \ 'recipients).read[Seq[MongoId]] or Reads.pure[Seq[MongoId]](Seq())) and
    Reads.pure[Seq[Message]](Seq()) and
    Reads.pure[Date](new Date) and
    Reads.pure[Date](new Date))(Conversation.apply _)

  def outputWrites(offset: Int, limit: Int) = Writes[Conversation] {
    c =>
      Json.obj("id" -> c.id.toJson) ++
        Json.obj("recipients" -> c.recipients.map(_.toJson)) ++
        Json.obj("messages" -> OutputLimits.applyLimits(c.messages.map(_.toJson), offset, limit)) ++
        Json.obj("numberOfMessages" -> c.messages.length) ++
        toJsonOrEmpty("subject", c.subject) ++
        addCreated(c.created) ++
        addLastUpdated(c.lastUpdated)
  }

  val summaryWrites = Writes[Conversation] {
    c =>
      Json.obj("id" -> c.id.toJson) ++
        addLastUpdated(c.lastUpdated) ++
        Json.obj("numberOfMessages" -> c.messages.length) ++
        toJsonOrEmpty("subject", c.subject) ++
        Json.obj("lastMessage" -> {
          c.messages.lastOption match {
            case Some(m) => m.messageBody
            case None    => ""
          }
        })
  }

  def create: Conversation = {
    val id = IdHelper.generateConversationId()
    new Conversation(id, None, Seq(), Seq(), new Date, new Date)
  }
}
