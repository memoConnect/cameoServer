package models

import play.api.libs.json._
import helper.JsonHelper._
import traits.Model
import java.util.Date
import helper.IdHelper
import scala.concurrent.{ ExecutionContext, Future }
import reactivemongo.core.commands.LastError
import ExecutionContext.Implicits.global
import helper.MongoCollections._

/**
 * User: Björn Reimer
 * Date: 1/15/14
 * Time: 10:51 AM
 */
case class FileMeta(id: MongoId,
                    chunks: Seq[ChunkMeta],
                    fileName: String,
                    maxChunks: Int,
                    fileSize: Int,
                    fileType: String,
                    created: Date) {

  def toJson: JsObject = Json.toJson(this)(FileMeta.outputWrites).as[JsObject]

  def addChunk(chunkMeta: ChunkMeta): Future[LastError] = {

    //upsert does not work on nested arrays in mongo. we need to get rid of all existing values before inserting
    val query = Json.obj("_id" -> this.id)
    val remove = Json.obj("$pull" -> Json.obj("chunks" -> Json.obj("index" -> chunkMeta.index)))
    FileMeta.col.update(query, remove).flatMap {
      lastError =>
        val add = Json.obj("$push" -> Json.obj("chunks" -> chunkMeta))
        FileMeta.col.update(query, add)
    }
  }

}

object FileMeta extends Model[FileMeta] {

  def col = fileMetaCollection

  implicit val mongoFormat: Format[FileMeta] = createMongoFormat[FileMeta](Json.reads[FileMeta], Json.writes[FileMeta])

  def docVersion = 0

  def evolutions = Map()

  val outputWrites: Writes[FileMeta] = Writes {
    fm =>
      Json.obj("id" -> fm.id.toJson) ++
        Json.obj("chunks" -> fm.chunks.map(_.toJson)) ++
        Json.obj("fileName" -> fm.fileName) ++
        Json.obj("maxChunks" -> fm.maxChunks) ++
        Json.obj("fileSize" -> fm.fileSize) ++
        Json.obj("fileType" -> fm.fileType) ++
        addCreated(fm.created)
  }

  def create(chunks: Seq[ChunkMeta], fileName: String, maxChunks: Int, fileSize: Int, fileType: String): FileMeta = {
    new FileMeta(
      IdHelper.generateFileId(),
      chunks,
      fileName,
      maxChunks,
      fileSize,
      fileType,
      new Date
    )
  }

  override def createDefault(): FileMeta = {
    new FileMeta(IdHelper.generateFileId(), Seq(), "filename", 0, 0, "none", new Date)
  }
}

case class ChunkMeta(index: Int, chunkId: MongoId, chunkSize: Int) {
  def toJson: JsNumber = JsNumber(index)
}

object ChunkMeta {
  implicit val mongoFormat: Format[ChunkMeta] = Json.format[ChunkMeta]
}

