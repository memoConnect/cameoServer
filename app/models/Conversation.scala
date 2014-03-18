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
                        recipients: Seq[Recipient],
                        messages: Seq[Message],
                        created: Date,
                        lastUpdated: Date) {

  def toJson(offset: Int = 0, limit: Int = 0): JsObject = Json.toJson(this)(Conversation.outputWrites(offset, limit)).as[JsObject]

  def toJson: JsObject = toJson(0, 0)

  def toSummaryJson: JsObject = Json.toJson(this)(Conversation.summaryWrites).as[JsObject]

  def toSummaryJsonWithRecipients: Future[JsObject] = {
    val recipients: Seq[Future[JsObject]] = this.recipients.map { _.toJsonWithIdentity }
    Future.sequence(recipients).map {
      r => this.toSummaryJson ++ Json.obj("recipients" -> r)
    }
  }

  def toSummaryJsonWithRecipientsResult: Future[SimpleResult] = {
    this.toSummaryJsonWithRecipients.map { resOK(_) }
  }

  def toJsonWithIdentities(offset: Int = 0, limit: Int = 0): Future[JsObject] = {
    val recipients: Seq[Future[JsObject]] = this.recipients.map { _.toJsonWithIdentity }

    Future.sequence(recipients).map {
      r => this.toJson(offset, limit) ++ Json.obj("recipients" -> r)
    }
  }

  def toJsonWithIdentitiesResult(offset: Int = 0, limit: Int = 0): Future[SimpleResult] = {
    this.toJsonWithIdentities(offset, limit).map { resOK(_) }
  }

  def query = Json.obj("_id" -> this.id)

  def setLastUpdated(js: JsObject): JsObject = {
    // check if there already is a $set block
    val set: JsValue = (js \ "$set").asOpt[JsValue] match {
      case None                => Json.obj("lastUpdated" -> new Date)
      case Some(obj: JsObject) => obj ++ Json.obj("lastUpdated" -> new Date)
      case Some(ar: JsArray)   => ar.append(Json.obj("lastUpdated" -> new Date))
      case Some(other)         => Logger.error("SetLastUpdated: Unable to process: " + js); other
    }
    js ++ Json.obj("$set" -> set)
  }

  def update(conversationUpdate: ConversationUpdate): Future[Boolean] = {
    val set = Json.obj("$set" -> maybeEmptyString("subject", conversationUpdate.subject))
    Conversation.col.update(query, setLastUpdated(set)).map { _.ok }
  }

  def addRecipients(recipients: Seq[Recipient]): Future[Boolean] = {
    val set = Json.obj("$addToSet" ->
      Json.obj("recipients" ->
        Json.obj("$each" -> recipients)))
    Conversation.col.update(query, setLastUpdated(set)).map { _.updatedExisting }
  }

  def deleteRecipient(recipient: Recipient): Future[Boolean] = {
    val set = Json.obj("$pull" ->
      Json.obj("recipients" -> recipient))
    Conversation.col.update(query, setLastUpdated(set)).map { _.updatedExisting }
  }

  def updateRecipient(recipient: Recipient, recipientUpdate: RecipientUpdate): Future[Boolean] = {
    val queryRecipient = query ++ Json.obj("recipients" -> Json.obj("$elemMatch" -> Json.obj("identityId" -> recipient.identityId)))
    val set = Json.obj("$set" -> Json.obj("recipients.$.encryptedKey" -> recipientUpdate.encryptedKey))
    Conversation.col.update(queryRecipient, setLastUpdated(set)).map { _.ok }
  }

  def hasMember(identityId: MongoId): Boolean = {
    this.recipients.exists(_.identityId.equals(identityId))
  }

  def hasMemberFutureResult(identityId: MongoId)(action: Future[SimpleResult]): Future[SimpleResult] = {
    if (this.hasMember(identityId)) {
      action
    } else {
      Future(resUnauthorized("identity is not a member of the conversation"))
    }
  }

  def hasMemberResult(identityId: MongoId)(action: SimpleResult): SimpleResult = {
    if (this.hasMember(identityId)) {
      action
    } else {
      resUnauthorized("identity is not a member of the conversation")
    }
  }

  def addMessage(message: Message): Future[LastError] = {
    val set = Json.obj("$push" ->
      Json.obj("messages" -> Json.obj(
        "$each" -> Seq(message),
        "$sort" -> Json.obj("created" -> 1),
        "$slice" -> (-1) * (this.messages.length + 2))))

    Conversation.col.update(query, setLastUpdated(set))
  }

  def getMessage(messageId: MongoId): Option[Message] = {
    this.messages.find(_.id.equals(messageId))
  }
}

object Conversation extends Model[Conversation] {

  def col: JSONCollection = conversationCollection

  implicit val mongoFormat: Format[Conversation] = createMongoFormat(Json.reads[Conversation], Json.writes[Conversation])

  def docVersion = 0

  def createReads = (
    Reads.pure[MongoId](IdHelper.generateConversationId()) and
    (__ \ 'subject).readNullable[String] and
    Reads.pure[Seq[Recipient]](Seq()) and
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
        maybeEmptyString("subject", c.subject) ++
        addCreated(c.created) ++
        addLastUpdated(c.lastUpdated)
  }

  val summaryWrites = Writes[Conversation] {
    c =>
      Json.obj("id" -> c.id.toJson) ++
        addLastUpdated(c.lastUpdated) ++
        Json.obj("numberOfMessages" -> c.messages.length) ++
        maybeEmptyString("subject", c.subject) ++
        Json.obj("messages" -> {
          c.messages.lastOption match {
            case Some(m) => Seq(m.toJson)
            case None    => Seq()
          }
        })
  }

  def findByMessageId(id: MongoId): Future[Option[Conversation]] = {
    col.find(arrayQuery("messages", id)).one[Conversation]
  }

  def findByIdentityId(id: MongoId): Future[Seq[Conversation]] = {
    val query = Json.obj("recipients" -> Json.obj("$elemMatch" -> Json.obj("identityId" -> id)))
    col.find(query).cursor[Conversation].collect[Seq]()
    // TODO: Reimplement this and return conversation summaries using the aggregation framework
  }

  def create: Conversation = {
    val id = IdHelper.generateConversationId()
    new Conversation(id, None, Seq(), Seq(), new Date, new Date)
  }

  def evolutions = Map()

  def createDefault(): Conversation = {
    new Conversation(IdHelper.generateConversationId(), None, Seq(), Seq(), new Date, new Date)
  }
}

case class ConversationUpdate(subject: Option[String])

object ConversationUpdate {
  implicit val format: Format[ConversationUpdate] = Json.format[ConversationUpdate]
}
