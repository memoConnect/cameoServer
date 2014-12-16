package models

import helper.{ JsonHelper, IdHelper }
import play.api.libs.json.{ Format, JsObject, Json, Writes }
import traits.SubModel

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

  implicit val mongoFormat: Format[Recipient] = createMongoFormat(Json.reads[Recipient], Json.writes[Recipient])

  def docVersion = 0
  def evolutions = Map()

  def outputWrites: Writes[Recipient] = Writes[Recipient] {
    r =>
      Json.obj("identityId" -> r.identityId.toJson) ++
        JsonHelper.maybeEmptyJson("keys", r.keys)
  }

  def create(identityId: MongoId, keys: Seq[RecipientKey] = Seq()): Recipient = {
    new Recipient(identityId, None, Some(keys))
  }

  def create(identityId: String, keys: Seq[RecipientKey] = Seq()): Recipient = {
    new Recipient(new MongoId(identityId), None, Some(keys))
  }

  override def createDefault(): Recipient = {
    new Recipient(IdHelper.generateRecipientId(), None, None)
  }
}

case class RecipientKey(id: String)
object RecipientKey { implicit val format = Json.format[RecipientKey] }