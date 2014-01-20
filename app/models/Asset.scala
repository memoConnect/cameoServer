package models

import java.util.Date
import traits.{OutputLimits, Model}
import play.api.libs.json._
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 6/29/13
 * Time: 3:04 PM
 */
case class Asset(
                  assetId: String,
                  filesize: String,
                  filename: String,
                  fileType: String,
                  created: Date
                  ) {

  def toJson: JsValue = Json.toJson(this)(Asset.outputWrites)

}

object Asset extends Model[Asset] {

  implicit val col = conversationCollection
  implicit val mongoFormat: Format[Asset] = createMongoFormat(Json.reads[Asset], Json.writes[Asset])

  // Input/output format for the API
  def inputReads: Reads[Asset] = Json.reads[Asset]

  def outputWrites(implicit ol: OutputLimits = OutputLimits(0, 0)): Writes[Asset] = Writes {
    asset =>
      Json.obj("assetId" -> asset.assetId) ++
        Json.obj("name" -> asset.filename) ++
        Json.obj("type" -> asset.fileType) ++
        Json.obj("size" -> asset.filesize) ++
        addCreated(asset.created)
  }

  def find(assetId: String): Future[Option[Asset]] = {
    val query = Json.obj("assetId" -> assetId)
    col.find(query).one[Asset]
  }

  override val sortWith = {
    (a1: Asset, a2: Asset) => a1.created.before(a2.created)
  }
}