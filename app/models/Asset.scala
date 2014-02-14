package models

import java.util.Date
import traits.{ Model }
import play.api.libs.json._
import scala.concurrent.{ ExecutionContext, Future }
import ExecutionContext.Implicits.global
import helper.JsonHelper._

/**
 * User: Björn Reimer
 * Date: 6/29/13
 * Time: 3:04 PM
 */
case class Asset(id: MongoId,
                 fileSize: String,
                 fileName: String,
                 fileType: String,
                 created: Date) {

  def toJson: JsObject = Json.toJson(this)(Asset.outputWrites).as[JsObject]

}

object Asset extends Model[Asset] {

  implicit val col = Conversation.col

  implicit val mongoFormat: Format[Asset] = createMongoFormat(Json.reads[Asset], Json.writes[Asset])

  def outputWrites: Writes[Asset] = Writes {
    asset =>
      Json.obj("id" -> asset.id.toJson) ++
        Json.obj("name" -> asset.fileName) ++
        Json.obj("type" -> asset.fileType) ++
        Json.obj("size" -> asset.fileSize) ++
        addCreated(asset.created)
  }
}