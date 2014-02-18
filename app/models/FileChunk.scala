package models

import play.modules.reactivemongo.json.collection.JSONCollection
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.libs.json.{Json, Format}
import play.api.Play.current
import traits.Model
import helper.JsonHelper._

/**
 * User: Björn Reimer
 * Date: 1/15/14
 * Time: 11:59 AM
 */
case class FileChunk(
                      id: MongoId,
                      chunk: String
                      )

object FileChunk extends Model[FileChunk]{

  // mongo collection
  def col = fileChunkCollection

  implicit val mongoFormat: Format[FileChunk] = createMongoFormat(Json.reads[FileChunk],Json.writes[FileChunk])

}
