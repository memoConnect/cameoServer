package models

import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.libs.json._
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.Play.current
import helper.JsonHelper._
import traits.Model
import java.util.Date

/**
 * User: Björn Reimer
 * Date: 1/15/14
 * Time: 10:51 AM
 */
case class FileMeta(id: MongoId,
                    chunks: Map[String, MongoId],
                    fileName: String,
                    maxChunks: Int,
                    fileSize: Int,
                    mimeType: String,
                    created: Date)

object FileMeta extends Model[FileMeta] {

  def col = fileMetaCollection

  implicit val mongoFormat: Format[FileMeta] = createMongoFormat[FileMeta](Json.reads[FileMeta], Json.writes[FileMeta])

}
