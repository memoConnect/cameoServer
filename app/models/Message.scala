package models

import java.util.Date

import helper.IdHelper
import helper.JsonHelper._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import traits.SubModel

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 2:36 PM
 */
case class Message(id: MongoId,
                   fromIdentityId: MongoId,
                   plain: Option[PlainMessagePart],
                   encrypted: Option[String],
                   signature: Option[MessageSignature],
                   created: Date,
                   docVersion: Int) {

  def toJson: JsObject = Json.toJson(this)(Message.outputWrites).as[JsObject]
}

object Message extends SubModel[Message, Conversation] {

  def parentModel = Conversation
  def elementName = "messages"

  implicit val mongoFormat: Format[Message] = createMongoFormat(Json.reads[Message], Json.writes[Message])

  def docVersion = 3

  def evolutions = Map(1 -> MessageEvolutions.splitPlainAndEncrypted, 2 -> MessageEvolutions.filesToFileIds)

  def createReads(fromIdentityId: MongoId) = (
    Reads.pure[MongoId](IdHelper.generateMessageId()) and
    Reads.pure[MongoId](fromIdentityId) and
    //    Reads.pure[Seq[MessageStatus]](Seq()) and
    (__ \ 'plain).readNullable[PlainMessagePart](PlainMessagePart.createReads) and
    (__ \ 'encrypted).readNullable[String] and
    (__ \ 'signature).readNullable[MessageSignature] and
    Reads.pure[Date](new Date) and
    Reads.pure[Int](docVersion)
  )(Message.apply _)

  def outputWrites = Writes[Message] {
    m =>
      Json.obj("id" -> m.id.toJson) ++
        Json.obj("fromIdentity" -> m.fromIdentityId.toJson) ++
        Json.obj("plain" -> m.plain.map(_.toJson)) ++
        Json.obj("encrypted" -> m.encrypted) ++
        maybeEmptyJson("signature", m.signature) ++
        addCreated(m.created)
  }

  def create(fromId: MongoId, text: String): Message = {
    new Message(IdHelper.generateMessageId(), fromId, Some(PlainMessagePart.create(text)), None, None,  new Date, docVersion)
  }

  override def createDefault(): Message = {
    create(MongoId(""), "")
  }
}

object MessageEvolutions {

  val assetsToFiles: Reads[JsObject] = Reads {
    js =>
      {
        val deleteAsset: Reads[JsObject] = (__ \ 'assets).json.prune
        val addFiles: Reads[JsObject] = __.json.update((__ \ 'files).json.put(JsArray()))
        val renameBody: Reads[JsObject] = __.json.update((__ \ 'body).json.copyFrom((__ \ 'messageBody).json.pick)) andThen (__ \ 'messageBody).json.prune
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(1)))
        js.transform(deleteAsset andThen addFiles andThen renameBody andThen addVersion)
      }
  }

  val splitPlainAndEncrypted: Reads[JsObject] = Reads {
    js =>
      {
        val deleteFiles = (__ \ 'files).json.prune // file not used yet, not need to move them
        val moveMessageBody = __.json.update((__ \ 'encrypted).json.copyFrom((__ \ 'body).json.pick)) andThen (__ \ 'body).json.prune
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(2)))
        val addEmptyFiles = __.json.update((__ \ 'plain \ 'files).json.put(JsArray()))

        js.transform(deleteFiles)
        js.transform(moveMessageBody)
        js.transform(addVersion andThen addEmptyFiles)
      }
  }

  val filesToFileIds: Reads[JsObject] = Reads {
    js =>
      {
        val deleteFiles: Reads[JsObject] = (__ \ 'plain \ 'files).json.prune
        val addFileIds: Reads[JsObject] = __.json.update((__ \ 'plain \ 'fileIds).json.put(JsArray()))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(3)))
        js.transform(deleteFiles)
        js.transform(addFileIds andThen addVersion)
      }
  }
}

case class PlainMessagePart(text: Option[String],
                            fileIds: Option[Seq[MongoId]]) {
  def toJson: JsObject = {
    maybeEmptyJson("text", this.text) ++
    maybeEmptyJson("fileIds", this.fileIds.map(_.map(_.toJson)))
  }
}

object PlainMessagePart {
  implicit val format: Format[PlainMessagePart] = Json.format[PlainMessagePart]

  val createReads = (
    (__ \ 'text).readNullable[String] and
    (__ \ 'fileIds).readNullable[Seq[MongoId]](Reads.seq(MongoId.createReads))
  )(PlainMessagePart.apply _)

  def create(text: String): PlainMessagePart = {
    new PlainMessagePart(Some(text), None)
  }
}

case class MessageSignature(isEncrypted: Boolean,
                             content: Seq[String])

object MessageSignature {
  implicit val format: Format[MessageSignature] = Json.format[MessageSignature]
}

