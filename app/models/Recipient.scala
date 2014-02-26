package models

import traits.Model
import play.api.libs.json.{Writes, Json}
import helper.JsonHelper._

/**
 * User: Björn Reimer
 * Date: 2/26/14
 * Time: 2:02 PM
 */

case class Recipient(id: MongoId, encryptedKey: Option[String]) {

}

object Recipient extends Model[Recipient] {

  def col = Conversation.col

  implicit val mongoFormat = createMongoFormat(Json.reads[Recipient], Json.writes[Recipient])

  def docVersion = 0
  def evolutions = Map()

  def outputWrites: Writes[Recipient] = Writes[Recipient] {
    r =>
      Json.obj("id" -> r.id.toJson) ++
      toJs
    "id"

  }





}
