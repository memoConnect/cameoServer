package models

import helper.{ JsonHelper, IdHelper }
import play.api.libs.json._
import traits.SubModel
import play.api.libs.functional.syntax._

/**
 * User: Björn Reimer
 * Date: 2/26/14
 * Time: 2:02 PM
 */

case class Recipient(identityId: MongoId,
                     messagesRead: Option[Int],
                     keys: Option[Seq[RecipientKey]]) {
  def toJson: JsObject = Json.toJson(this)(Recipient.outputWrites).as[JsObject]
}

object Recipient extends SubModel[Recipient, Conversation] {

  def parentModel = Conversation
  def elementName = "recipients"

  override val idName = "identityId"

  implicit val mongoKeyFormat = RecipientKey.format
  implicit val mongoFormat: Format[Recipient] = createMongoFormat(Json.reads[Recipient], Json.writes[Recipient])

  def evolutions = Map()

  def outputWrites: Writes[Recipient] = Writes[Recipient] {
    r =>
      Json.obj("identityId" -> r.identityId.toJson) ++
        JsonHelper.maybeEmptyJson("keys", r.keys)
  }

  def createReads: Reads[Recipient] = (
    (__ \ 'identityId).read[MongoId](MongoId.createReads) and
    Reads.pure[Option[Int]](None) and
    (__ \ 'keys).readNullable[Seq[RecipientKey]]
  )(Recipient.apply _)

  def create(identityId: MongoId, keys: Seq[RecipientKey] = Seq()): Recipient = {
    val keysOption = if (keys.isEmpty) None else Some(keys)
    new Recipient(identityId, None, keysOption)
  }

  override def createDefault(): Recipient = {
    new Recipient(IdHelper.generateRecipientId(), None, None)
  }
}

case class RecipientKey(id: String)
object RecipientKey { implicit val format = Json.format[RecipientKey] }