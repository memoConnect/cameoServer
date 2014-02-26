package models

import traits.Model
import play.api.libs.json.{Format, JsObject, Writes, Json}
import helper.JsonHelper._
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 2/26/14
 * Time: 2:02 PM
 */

case class Recipient(identityId: MongoId, encryptedKey: Option[String]) {

  def toJson: JsObject = Json.toJson(this)(Recipient.outputWrites).as[JsObject]

  def toJsonWithIdentity: Future[JsObject] = {
    Identity.find(this.identityId).map {
      case None    => Json.obj()
      case Some(i) => Json.obj("identity" -> i.toSummaryJson) ++ this.toJson
    }
  }
}

object Recipient extends Model[Recipient] {

  def col = Conversation.col

  implicit val mongoFormat: Format[Recipient] = createMongoFormat(Json.reads[Recipient], Json.writes[Recipient])

  def docVersion = 0
  def evolutions = Map()

  def outputWrites: Writes[Recipient] = Writes[Recipient] {
    r =>
      Json.obj("identityId" -> r.identityId.toJson) ++
      toJsonOrEmpty("encryptedKey", r.encryptedKey)
  }

  def create(identityId: MongoId): Recipient = {
    new Recipient(identityId, None)
  }

  def create(identityId: String): Recipient = {
    new Recipient(new MongoId(identityId), None)
  }



}

case class RecipientUpdate(encryptedKey: String)

object RecipientUpdate {
  implicit val format = Json.format[RecipientUpdate]
}
