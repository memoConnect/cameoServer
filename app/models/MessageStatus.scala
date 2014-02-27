package models

import play.api.libs.json.{ JsObject, Writes, Format, Json }
import helper.JsonHelper._
import traits.Model
import scala.concurrent.Future
import reactivemongo.core.commands.LastError

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

  def col = Message.col

  implicit val mongoFormat: Format[MessageStatus] = createMongoFormat(Json.reads[MessageStatus], Json.writes[MessageStatus])

  def docVersion = 0
  def evolutions = Map()

  val outputWrites: Writes[MessageStatus] = Writes {
    ms =>
      Json.obj("identityId" -> ms.identityId.toJson) ++
        Json.obj("status" -> ms.status) ++
        Json.obj("message" -> ms.message)
  }
}
