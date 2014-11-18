package models

import helper.IdHelper
import play.api.libs.json.{ Format, JsObject, Json, Writes }
import traits.SubModel

/**
 * User: Björn Reimer
 * Date: 2/26/14
 * Time: 2:02 PM
 */

case class Recipient(identityId: MongoId,
                     lastMessageRead: Option[MongoId]) {
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
      Json.obj("identityId" -> r.identityId.toJson)
  }

  def create(identityId: MongoId): Recipient = {
    new Recipient(identityId, None)
  }

  def create(identityId: String): Recipient = {
    new Recipient(new MongoId(identityId), None)
  }

  override def createDefault(): Recipient = {
    new Recipient(IdHelper.generateRecipientId(), None)
  }
}