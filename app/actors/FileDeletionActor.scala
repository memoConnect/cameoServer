package actors

import java.util.Date

import akka.actor.Actor
import models.{FileChunk, FileMeta}
import org.joda.time.DateTime
import play.api.{ Logger, Play }
import play.api.libs.json.Json
 import scala.concurrent.ExecutionContext.Implicits.global
/**
 * User: Björn Reimer
 * Date: 27.10.14
 * Time: 12:34
 */

case class DeleteFiles(lifetime: Int)

class FileDeletionActor extends Actor {

  def receive = {
    case DeleteFiles(lifetime) =>

      Logger.debug("starting delete file actor")

      val deleteBefore = new DateTime().minusDays(lifetime)

      val query = Json.obj("created" -> Json.obj("$lt" -> Json.obj("$date" -> deleteBefore)))

      FileMeta.findAll(query).map {
        fileMetaList =>
          val chunks = fileMetaList.flatMap(_.chunks).map(_.chunkId.id)
          val fileMetaIds = fileMetaList.map(fm => Json.obj("_id" -> fm.id))
          // delete file chunks
          Logger.debug("Deleting " + chunks.length + " file chunks")
          FileChunk.deleteAll(chunks)

          // delete file meta documents
          Logger.debug("Deleting " + fileMetaList.length + " files\n")
          val fileMetaDelete = Json.obj("$or" -> fileMetaIds)
          FileMeta.deleteAll(fileMetaDelete)
      }
  }
}
