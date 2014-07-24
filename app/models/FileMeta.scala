package models

import java.util.Date

import helper.IdHelper
import helper.JsonHelper._
import helper.MongoCollections._
import play.api.libs.json._
import reactivemongo.core.commands.LastError
import traits.Model

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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
                    isCompleted: Boolean,
                    created: Date,
                    docVersion: Int) {

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

  def setCompleted(value: Boolean): Future[Boolean] = {
    val query = Json.obj("_id" -> this.id)
    val set = Json.obj("$set" -> Json.obj("isCompleted" -> value))
    FileMeta.col.update(query, set).map(_.updatedExisting)
  }

}

object FileMeta extends Model[FileMeta] {

  def col = fileMetaCollection

  implicit val mongoFormat: Format[FileMeta] = createMongoFormat(Json.reads[FileMeta], Json.writes[FileMeta])

  def docVersion = 1

  def evolutions = Map(0 -> FileMetaEvolutions.addCompletedFlag)

  val outputWrites: Writes[FileMeta] = Writes {
    fm =>
      Json.obj("id" -> fm.id.toJson) ++
        Json.obj("chunks" -> fm.chunks.map(_.toJson)) ++
        Json.obj("fileName" -> fm.fileName) ++
        Json.obj("maxChunks" -> fm.maxChunks) ++
        Json.obj("fileSize" -> fm.fileSize) ++
        Json.obj("fileType" -> fm.fileType) ++
        Json.obj("isCompleted" -> fm.isCompleted) ++
        addCreated(fm.created)
  }

  def create(chunks: Seq[ChunkMeta], fileName: String, maxChunks: Int, fileSize: Int, fileType: String, isCompleted: Boolean = false, conversationId: Option[MongoId] = None): FileMeta = {
    new FileMeta(
      IdHelper.generateFileId(),
      chunks,
      fileName,
      maxChunks,
      fileSize,
      fileType,
      isCompleted,
      new Date,
      docVersion
    )
  }

  override def createDefault(): FileMeta = {
    new FileMeta(IdHelper.generateFileId(), Seq(), "filename", 0, 0, "none", false, new Date, docVersion)
  }
}

object FileMetaEvolutions {
  def addCompletedFlag(): Reads[JsObject] = Reads {
    js =>
      {
        val addFlag: Reads[JsObject] = __.json.update((__ \ 'isCompleted).json.put(JsBoolean(value = true)))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(1)))
        js.transform(addFlag andThen addVersion)
      }
  }
}

case class ChunkMeta(index: Int, chunkId: MongoId, chunkSize: Int) {
  def toJson: JsNumber = JsNumber(index)
}

object ChunkMeta {
  implicit val mongoFormat: Format[ChunkMeta] = Json.format[ChunkMeta]
}

