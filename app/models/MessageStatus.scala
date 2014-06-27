package models

import helper.{ IdHelper, MongoCollections }
import play.api.libs.json.{ Format, JsObject, Json, Writes }
import traits.Model

/**
 * User: Björn Reimer
 * Date: 1/22/14
 * Time: 3:11 PM
 */
case class MessageStatus(identityId: MongoId,
                         status: String,
                         message: String) {

  def toJson: JsObject = {
    Json.toJson[MessageStatus](this)(MessageStatus.outputWrites).as[JsObject]
  }
}

object MessageStatus extends Model[MessageStatus] {

  def col = MongoCollections.conversationCollection

  implicit val mongoFormat: Format[MessageStatus] = createMongoFormat(Json.reads[MessageStatus], Json.writes[MessageStatus])

  def docVersion = 0
  def evolutions = Map()

  val outputWrites: Writes[MessageStatus] = Writes {
    ms =>
      Json.obj("identityId" -> ms.identityId.toJson) ++
        Json.obj("status" -> ms.status) ++
        Json.obj("message" -> ms.message)
  }

  override def createDefault(): MessageStatus = {
    new MessageStatus(IdHelper.generateMongoId(), "", "")
  }
}
