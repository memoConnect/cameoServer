package models

import java.util.Date
import traits.Model
import scala.concurrent.{ ExecutionContext, Future }
import helper.{ MongoCollections, IdHelper }
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import ExecutionContext.Implicits.global
import reactivemongo.core.commands._
import helper.ResultHelper._
import helper.JsonHelper._
import helper.MongoCollections._
import play.api.Logger
import reactivemongo.bson.BSONNull
import play.modules.reactivemongo.json.BSONFormats._
import play.api.libs.json.JsArray
import play.modules.reactivemongo.json.collection.JSONCollection
import scala.Some
import play.api.mvc.SimpleResult
import play.api.libs.json.JsNumber
import reactivemongo.core.commands.Unwind
import play.api.libs.json.JsObject

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 1:29 PM
 */

case class Conversation(id: MongoId,
                        subject: Option[String],
                        recipients: Seq[Recipient],
                        messages: Seq[Message],
                        encPassList: Seq[EncryptedPassphrase],
                        passCaptcha: Option[MongoId],
                        numberOfMessages: Option[Int],
                        created: Date,
                        lastUpdated: Date,
                        docVersion: Int) {

  def toJson: JsObject = Json.toJson(this)(Conversation.outputWrites).as[JsObject]

  def toSummaryJson: Future[JsObject] =
    getMessageCount.map { count =>
      Json.toJson(this)(Conversation.summaryWrites).as[JsObject] ++
        Json.obj("numberOfMessages" -> count)
    }

  def toSummaryJsonWithIdentities: Future[JsObject] = {
    val recipients: Seq[Future[JsObject]] = this.recipients.map { _.toJsonWithIdentity }

    for {
      recipientsWithIdentities <- Future.sequence(recipients)
      summary <- this.toSummaryJson
    } yield {
      summary ++
        Json.obj("recipients" -> recipientsWithIdentities)
    }
  }

  def toSummaryJsonWithIdentitiesResult: Future[SimpleResult] = {
    this.toSummaryJsonWithIdentities.map { resOK(_) }
  }

  def toJsonWithIdentities: Future[JsObject] = {
    val recipients: Seq[Future[JsObject]] = this.recipients.map { _.toJsonWithIdentity }
    Future.sequence(recipients).map {
      r => this.toJson ++ Json.obj("recipients" -> r)
    }
  }

  def toJsonWithIdentitiesResult: Future[SimpleResult] = {
    this.toJsonWithIdentities.map { resOK(_) }
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

  def getMessage(messageId: MongoId): Option[Message] = {
    this.messages.find(_.id.equals(messageId))
  }

  def getMessageCount: Future[Int] = {

    val pipeline: Seq[PipelineOperator] = Seq(
      Match(toBson(query).get),
      Unwind("messages"),
      Group(BSONNull)(("count", SumValue(1))))

    val command = Aggregate(Conversation.col.name, pipeline)

    MongoCollections.mongoDB.command(command).map {
      res =>
        res.headOption match {
          case None => 0
          case Some(bson) =>
            val js = Json.toJson(bson)
            (js \ "count").as[Int]
        }
    }
  }

  def update(conversationUpdate: ConversationUpdate): Future[Boolean] = {
    val set =
      Json.obj("$set" -> (
        maybeEmptyString("subject", conversationUpdate.subject) ++
        maybeEmptyJsValue("passCaptcha", conversationUpdate.passCaptcha.map(str => Json.toJson(MongoId(str)))))
      )
    Conversation.col.update(query, set).map { _.ok }
  }

  def addRecipients(recipients: Seq[Recipient]): Future[Boolean] = {
    Recipient.appendUnique(this.id, recipients).map(_.updatedExisting)
  }

  def deleteRecipient(identityId: MongoId): Future[Boolean] = {
    Recipient.delete(this.id, identityId).map(_.updatedExisting)
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
    val set = Json.obj("$push" -> Json.obj("messages" -> message))
    Conversation.col.update(query, setLastUpdated(set))
  }

  def setEncPassList(list: Seq[EncryptedPassphrase]): Future[Boolean] = {
    val set = Json.obj("$set" ->
      Json.obj("encPassList" -> list))
    Conversation.col.update(query, set).map(_.updatedExisting)
  }

  def setPassCaptcha(fileId: MongoId): Future[Boolean] = {
    val set = Json.obj("$set" -> Json.obj("passCaptcha" -> fileId))
    Conversation.col.update(query, set).map(_.updatedExisting)
  }

}

object Conversation extends Model[Conversation] {

  def col: JSONCollection = conversationCollection

  implicit val mongoFormat: Format[Conversation] = createMongoFormat(Json.reads[Conversation], Json.writes[Conversation])

  def docVersion = 1

  def createReads = (
    Reads.pure[MongoId](IdHelper.generateConversationId()) and
    (__ \ 'subject).readNullable[String] and
    Reads.pure[Seq[Recipient]](Seq()) and
    Reads.pure[Seq[Message]](Seq()) and
    Reads.pure[Seq[EncryptedPassphrase]](Seq()) and
    Reads.pure[Option[MongoId]](None) and
    Reads.pure[Option[Int]](None) and
    Reads.pure[Date](new Date) and
    Reads.pure[Date](new Date) and
    Reads.pure[Int](docVersion)
  )(Conversation.apply _)

  def outputWrites = Writes[Conversation] {
    c =>
      Json.obj("id" -> c.id.toJson) ++
        Json.obj("recipients" -> c.recipients.map(_.toJson)) ++
        Json.obj("messages" -> c.messages.map(_.toJson)) ++
        Json.obj("encryptedPassphraseList" -> c.encPassList.map(_.toJson)) ++
        maybeEmptyString("subject", c.subject) ++
        maybeEmptyString("passCaptcha", c.passCaptcha.map(_.toString)) ++
        addCreated(c.created) ++
        addLastUpdated(c.lastUpdated)
  }

  val summaryWrites = Writes[Conversation] {
    c =>
      Json.obj("id" -> c.id.toJson) ++
        addLastUpdated(c.lastUpdated) ++
        maybeEmptyString("subject", c.subject) ++
        Json.obj("encryptedPassphraseList" -> c.encPassList.map(_.toJson)) ++
        Json.obj("messages" -> c.messages.map(_.toJson)) ++
        maybeEmptyString("passCaptcha", c.passCaptcha.map(_.toString))

  }

  override def find(id: MongoId): Future[Option[Conversation]] = {
    find(id, -1, 0)
  }

  def find(id: String, limit: Int, offset: Int): Future[Option[Conversation]] = {
    find(new MongoId(id), limit, offset)
  }

  def find(id: MongoId, limit: Int, offset: Int): Future[Option[Conversation]] = {
    val query = Json.obj("_id" -> id)
    val projection = limitArray("messages", limit, offset)
    col.find(query, projection).one[Conversation]
  }

  def findByMessageId(id: MongoId, limit: Int, offset: Int): Future[Option[Conversation]] = {
    col.find(arrayQuery("messages", id), limitArray("messages", limit, offset)).one[Conversation]
  }

  def findByIdentityId(id: MongoId): Future[Seq[Conversation]] = {
    val query = Json.obj("recipients" -> Json.obj("identityId" -> id))
    col.find(query, limitArray("messages", -1, 0)).cursor[Conversation].collect[Seq]()
  }

  def create: Conversation = {
    val id = IdHelper.generateConversationId()
    new Conversation(id, None, Seq(), Seq(), Seq(), None, None, new Date, new Date, 0)
  }

  def evolutions = Map(
    0 -> ConversationEvolutions.addEncPassList
  )

  def createDefault(): Conversation = {
    new Conversation(IdHelper.generateConversationId(), None, Seq(), Seq(), Seq(), None, None, new Date, new Date, 0)
  }
}

case class ConversationUpdate(subject: Option[String],
                              passCaptcha: Option[String])

object ConversationUpdate {
  implicit val format: Format[ConversationUpdate] = Json.format[ConversationUpdate]
}

object ConversationEvolutions {

  val addEncPassList: Reads[JsObject] = Reads {
    js =>
      {
        val addEmptyList: Reads[JsObject] = __.json.update((__ \ 'encPassList).json.put(JsArray()))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(1)))
        js.transform(addEmptyList andThen addVersion)
      }
  }

}
