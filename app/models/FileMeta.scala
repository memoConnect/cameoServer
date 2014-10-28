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
                    scaleCache: Map[String, String], // Map: Scale size -> id of cached file
                    owner: Option[MongoId],
                    created: Date,
                    docVersion: Int) {

  def toJson: JsObject = Json.toJson(this)(FileMeta.outputWrites).as[JsObject]

  val query = Json.obj("_id" -> this.id)

  def addChunk(chunkMeta: ChunkMeta): Future[LastError] = {
    //upsert does not work on nested arrays in mongo. So we need to get rid of all existing values before inserting
    val remove = Json.obj("$pull" -> Json.obj("chunks" -> Json.obj("index" -> chunkMeta.index)))
    FileMeta.col.update(query, remove).flatMap {
      lastError =>
        val add = Json.obj("$push" -> Json.obj("chunks" -> chunkMeta))
        FileMeta.col.update(query, add)
    }
  }

  def setCompleted(value: Boolean): Future[Boolean] = {
    val set = Json.obj("$set" -> Json.obj("isCompleted" -> value))
    FileMeta.update(this.id, set)
  }

  def addToScaleCache(size: String, id: String): Future[Boolean] = {
    val set = Json.obj("$set" -> Json.obj(("scaleCache." + size) -> id))
    FileMeta.update(this.id, set)
  }


}

object FileMeta extends Model[FileMeta] {

  def col = fileMetaCollection

  implicit val mongoFormat: Format[FileMeta] = createMongoFormat(Json.reads[FileMeta], Json.writes[FileMeta])

  def docVersion = 2

  def evolutions = Map(
    0 -> FileMetaEvolutions.addCompletedFlag,
    1 -> FileMetaEvolutions.addScaleCache()
  )

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
      Map(),
      None,
      new Date,
      docVersion
    )
  }

  def setOwner(id: MongoId, identityId: MongoId): Future[Boolean] = {
    val set = Json.obj("$set" -> Json.obj("owner" -> identityId))
    FileMeta.update(id, set)
  }

  def deleteWithChunks(id: MongoId): Future[Boolean] = {
    // find chunks first
    FileMeta.find(id).flatMap {
      case None => Future(false)
      case Some(fileMeta) =>
        fileMeta.chunks.map {
          chunk => FileChunk.delete(chunk.chunkId.toString)
        }
        delete(id).map(_.updatedExisting)
    }
  }

  def createDefault(): FileMeta = {
    new FileMeta(IdHelper.generateFileId(), Seq(), "filename", 0, 0, "none", false, Map(), None, new Date, docVersion)
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

  def addScaleCache(): Reads[JsObject] = Reads {
    js =>
      {
        val addObject: Reads[JsObject] = __.json.update((__ \ 'scaleCache).json.put(Json.obj()))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(2)))
        js.transform(addObject andThen addVersion)
      }
  }
}

case class ChunkMeta(index: Int, chunkId: MongoId, chunkSize: Int) {
  def toJson: JsNumber = JsNumber(index)
}

object ChunkMeta {
  implicit val mongoFormat: Format[ChunkMeta] = Json.format[ChunkMeta]
}

