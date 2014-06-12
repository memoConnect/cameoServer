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
import play.api.mvc.Result
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
                        aePassphraseList: Seq[EncryptedPassphrase],
                        sePassphrase: Option[String],
                        passCaptcha: Option[MongoId],
                        numberOfMessages: Option[Int],
                        created: Date,
                        lastUpdated: Date,
                        docVersion: Int) {

  def toJson: JsObject = Json.toJson(this)(Conversation.outputWrites).as[JsObject]

  def getPassphraseList(keyIds: Seq[String]): JsObject = {
    val list = aePassphraseList.filter(passphrase => keyIds.contains(passphrase.keyId))
    Json.obj("aePassphraseList" -> list.map(_.toJson))
  }

  def toJsonWithKey(keyIds: Seq[String]): JsObject = {
    this.toJson ++ getPassphraseList(keyIds)
  }

  def toSummaryJson: Future[JsObject] =
    getMessageCount.map { count =>
      Json.toJson(this)(Conversation.summaryWrites).as[JsObject] ++
        Json.obj("numberOfMessages" -> count)
    }

  def toSummaryJsonWithKey(keyIds: Seq[String]): Future[JsObject] = {
    this.toSummaryJson.map { js =>
      js ++ getPassphraseList(keyIds)
    }

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
        maybeEmptyJsValue("passCaptcha", conversationUpdate.passCaptcha.map(str => Json.toJson(MongoId(str)))) ++
        maybeEmptyString("sePassphrase", conversationUpdate.sePassphrase) ++
        maybeEmptyJsValue("aePassphraseList", conversationUpdate.aePassphraseList.map(Json.toJson(_)))
      ))
    Conversation.col.update(query, set).map { _.ok }
  }

  def addRecipients(recipients: Seq[Recipient]): Future[Boolean] = {
    Recipient.appendUnique(this.id, recipients).map(_.updatedExisting)
  }

  def deleteRecipient(identityId: MongoId): Future[Boolean] = {
    Recipient.delete(this.id, identityId).map(_.updatedExisting)
  }

  def hasMember(identityId: MongoId): Boolean = {
    val res = this.recipients.exists(_.identityId.equals(identityId))
    res
  }

  def hasMemberResult(identityId: MongoId)(action: => Result): Result = {
    this.hasMember(identityId) match {
      case true  => action
      case false => resUnauthorized("identity is not a member of the conversation")
    }
  }

  def hasMemberFutureResult(identityId: MongoId)(action: => Future[Result]): Future[Result] = {
    this.hasMember(identityId) match {
      case true  => action
      case false => Future(resUnauthorized("identity is not a member of the conversation"))
    }
  }

  def addMessage(message: Message): Future[Boolean] = {
    val set = Json.obj("$push" -> Json.obj("messages" -> message))
    Conversation.col.update(query, setLastUpdated(set)).map(_.updatedExisting)
  }

  def addAePassphrases(aePassphrases: Seq[EncryptedPassphrase]): Future[Boolean] = {
    EncryptedPassphrase.appendUnique(this.id, aePassphrases).map(_.updatedExisting)
  }

  case class MissingPassphrase(identityId: String, keyId: String)
  object MissingPassphrase { implicit val format = Json.format[MissingPassphrase] }

  def getMissingPassphrases: Future[Seq[MissingPassphrase]] = {

    // get keyIds of all recipients. Todo: this takes quite a lot of db lookups, reduce!
    val futureKeys: Seq[Future[Seq[MissingPassphrase]]] = this.recipients.map {
      recipient =>
        Identity.find(recipient.identityId).map {
          case None => Seq()
          case Some(identity) =>
            val filtered = identity.publicKeys.filterNot(pubKey => this.aePassphraseList.exists(_.keyId.equals(pubKey.id.id)))
            filtered.map(pubKey => new MissingPassphrase(identity.id.id, pubKey.id.id))
        }
    }
    Future.sequence(futureKeys).map(_.flatten)
  }
}

object Conversation extends Model[Conversation] {

  def col: JSONCollection = conversationCollection

  implicit val mongoFormat: Format[Conversation] = createMongoFormat(Json.reads[Conversation], Json.writes[Conversation])

  def docVersion = 3

  def outputWrites = Writes[Conversation] {
    c =>
      Json.obj("id" -> c.id.toJson) ++
        Json.obj("recipients" -> c.recipients.map(_.toJson)) ++
        Json.obj("messages" -> c.messages.map(_.toJson)) ++
        Json.obj("aePassphraseList" -> c.aePassphraseList.map(_.toJson)) ++
        maybeEmptyString("sePassphrase", c.sePassphrase) ++
        maybeEmptyString("subject", c.subject) ++
        maybeEmptyString("passCaptcha", c.passCaptcha.map(_.toString)) ++
        addCreated(c.created) ++
        addLastUpdated(c.lastUpdated)
  }

  val summaryWrites = Writes[Conversation] {
    c =>
      Json.obj("id" -> c.id.toJson) ++
        addLastUpdated(c.lastUpdated) ++
        Json.obj("recipients" -> c.recipients.map(_.toJson)) ++
        maybeEmptyString("subject", c.subject) ++
        Json.obj("messages" -> c.messages.map(_.toJson)) ++
        maybeEmptyString("sePassphrase", c.sePassphrase) ++
        maybeEmptyString("passCaptcha", c.passCaptcha.map(_.toString))
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

  def create(subject: Option[String] = None,
             recipients: Seq[Recipient] = Seq(),
             passCaptcha: Option[String] = None,
             aePassphraseList: Option[Seq[EncryptedPassphrase]] = None,
             sePassphrase: Option[String] = None): Conversation = {
    val id = IdHelper.generateConversationId()
    new Conversation(id, subject, recipients, Seq(), aePassphraseList.getOrElse(Seq()), sePassphrase, passCaptcha.map(new MongoId(_)), None, new Date, new Date, 0)
  }

  def evolutions = Map(
    0 -> ConversationEvolutions.addEncPassList,
    1 -> ConversationEvolutions.renameEncPassList,
    2 -> ConversationEvolutions.fixNameMixup
  )

  def createDefault(): Conversation = {
    new Conversation(IdHelper.generateConversationId(), None, Seq(), Seq(), Seq(), None, None, None, new Date, new Date, 0)
  }
}

case class ConversationUpdate(subject: Option[String],
                              passCaptcha: Option[String],
                              aePassphraseList: Option[Seq[EncryptedPassphrase]],
                              sePassphrase: Option[String])

object ConversationUpdate {
  implicit val format: Format[ConversationUpdate] = Json.format[ConversationUpdate]

  val createReads: Reads[ConversationUpdate] = (
    (__ \ "subject").readNullable[String] and
    (__ \ "passCaptcha").readNullable[String] and
    (__ \ "aePassphraseList").readNullable(Reads.seq(EncryptedPassphrase.createReads)) and
    (__ \ "sePassphrase").readNullable[String]
  )(ConversationUpdate.apply _)
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

  val renameEncPassList: Reads[JsObject] = Reads {
    js =>
      {
        val rename = __.json.update((__ \ 'sePassphraseList).json.copyFrom((__ \ 'encPassList).json.pick)) andThen (__ \ 'encPassList).json.prune
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(2)))
        js.transform(rename andThen addVersion)
      }
  }

  val fixNameMixup: Reads[JsObject] = Reads {
    js =>
      {
        {
          val renameAePassphraseList = __.json.update((__ \ 'aePassphraseList).json.copyFrom((__ \ 'sePassphraseList).json.pick)) andThen (__ \ 'sePassphraseList).json.prune
          val removeAePassphrase = (__ \ 'aePassphrase).json.prune
          val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(3)))
          js.transform(renameAePassphraseList andThen removeAePassphrase andThen addVersion)
        }
      }
  }

}
