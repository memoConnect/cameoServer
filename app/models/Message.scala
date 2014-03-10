package models

import java.util.Date
import traits.{ Model }
import play.api.libs.json._
import helper.{ MongoCollections, IdHelper }
import play.api.libs.functional.syntax._
import scala.concurrent.{ ExecutionContext, Future }
import ExecutionContext.Implicits.global
import helper.JsonHelper._
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.core.commands.LastError

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 2:36 PM
 */
case class Message(id: MongoId,
                   body: String,
                   fromIdentityId: MongoId,
                   messageStatus: Seq[MessageStatus],
                   files: Seq[MongoId],
                   created: Date) {

  def toJson: JsObject = Json.toJson(this)(Message.outputWrites).as[JsObject]

  def updateAllStatus(messageStatus: Seq[MessageStatus]) = {
    val update = Json.obj("$set" -> Json.obj("messages.$.messageStatus" -> messageStatus))
    Conversation.col.update(arrayQuery("messages", this.id), update)
  }

  def updateSingleStatus(status: MessageStatus): Future[Boolean] = {
    // first remove old status (mongo cant update nested arrays...)
    val query = arrayQuery("messages", this.id)
    val set = Json.obj("$pull" -> Json.obj("messages.$.messageStatus" -> Json.obj("identityId" -> status.identityId)))

    Message.col.update(query, set).flatMap { lastError =>
      lastError.ok match {
        case false => Future(false)
        case true => {
          // write new message status
          val set2 = Json.obj("$push" -> Json.obj("messages.$.messageStatus" -> status))
          Message.col.update(query, set2).map { _.ok }
        }
      }
    }
  }
}

object Message extends Model[Message] {

  val col = MongoCollections.conversationCollection
  implicit val mongoFormat: Format[Message] = createMongoFormat(Json.reads[Message], Json.writes[Message])

  def docVersion = 1
  def evolutions = Map(0 -> MessageEvolutions.assetsToFiles)

  def createReads(fromIdentityId: MongoId) = (
    Reads.pure[MongoId](IdHelper.generateMessageId()) and
    (__ \ 'body).read[String] and
    Reads.pure[MongoId](fromIdentityId) and
    Reads.pure[Seq[MessageStatus]](Seq()) and
    ((__ \ 'fileIds).read[Seq[MongoId]](Reads.seq(MongoId.createReads)) or Reads.pure[Seq[MongoId]](Seq())) and
    Reads.pure[Date](new Date))(Message.apply _)

  def outputWrites = Writes[Message] {
    m =>
      Json.obj("id" -> m.id.toJson) ++
        Json.obj("body" -> m.body) ++
        Json.obj("fromIdentity" -> m.fromIdentityId.toJson) ++
        Json.obj("messageStatus" -> m.messageStatus.map(_.toJson)) ++
        Json.obj("fileIds" -> m.files.map(_.toJson)) ++
        addCreated(m.created)
  }

  override def find(id: MongoId): Future[Option[Message]] = {

    val projection = Json.obj("messages" -> Json.obj("$elemMatch" -> Json.obj("_id" -> id)))

    Conversation.col.find(arrayQuery("messages", id), projection).one[JsValue].map {
      case None     => None
      case Some(js) => Some((js \ "messages")(0).as[Message])
    }
  }

  override def save(js: JsObject): Future[LastError] = {
    val id: MongoId = (js \ "_id").as[MongoId]
    val query = arrayQuery("messages", id)
    val set = Json.obj("$set" -> Json.obj("messages.$" -> js))
    col.update(query, set)
  }

  def findConversation(id: MongoId): Future[Option[Conversation]] = {
    Conversation.col.find(arrayQuery("messages", id)).one[Conversation]
  }
}

object MessageEvolutions {

  val assetsToFiles: Reads[JsObject] = Reads {
    js =>
    {
      val deleteAsset: Reads[JsObject] = (__ \ 'assets).json.prune
      val addFiles: Reads[JsObject] = __.json.update((__ \ 'files).json.put(JsArray()))
      val renameBody: Reads[JsObject]  = __.json.update((__ \ 'body).json.copyFrom((__ \ 'messageBody).json.pick)) andThen (__ \ 'messageBody).json.prune
      val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(1)))
      js.transform(deleteAsset andThen addFiles andThen renameBody andThen addVersion)
    }
  }

}

