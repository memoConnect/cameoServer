package models

import traits.{ SubModel, Model }
import play.api.libs.json.{ Format, JsObject, Writes, Json }
import helper.JsonHelper._
import scala.concurrent.{ ExecutionContext, Future }
import ExecutionContext.Implicits.global
import reactivemongo.core.commands.LastError
import helper.{ IdHelper, MongoCollections }

/**
 * User: Björn Reimer
 * Date: 2/26/14
 * Time: 2:02 PM
 */

case class Recipient(identityId: MongoId) {

  def toJson: JsObject = Json.toJson(this)(Recipient.outputWrites).as[JsObject]

  def toJsonWithIdentity: Future[JsObject] = {
    Identity.find(this.identityId).map {
      case None    => Json.obj()
      case Some(i) => Json.obj("identity" -> i.toPublicJson) ++ this.toJson
    }
  }
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
    new Recipient(identityId)
  }

  def create(identityId: String): Recipient = {
    new Recipient(new MongoId(identityId))
  }

  override def createDefault(): Recipient = {
    new Recipient(IdHelper.generateRecipientId())
  }
}