package models

import helper.MongoCollections._
import scala.concurrent.{ ExecutionContext, Future }
import reactivemongo.core.commands.LastError
import reactivemongo.bson.{ Subtype, BSONBinary, BSONDocument }
import ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 1/15/14
 * Time: 11:59 AM
 */
object FileChunk {

  def col = fileChunkBsonCollection

  def insert(id: String, data: Array[Byte]): Future[LastError] = {
    val bson: BSONDocument = BSONDocument(
      "_id" -> id,
      "data" -> BSONBinary(data, Subtype.GenericBinarySubtype)
    )
    col.insert(bson)
  }

  def find(id: String): Future[Option[Array[Byte]]] = {
    col.find(BSONDocument("_id" -> id)).one[BSONDocument].map {
      _.flatMap {
        chunk =>
          chunk.getAs[BSONBinary]("data").map {
            data => data.value.readArray(data.value.size)
          }
      }
    }
  }

  def delete(id: String): Future[LastError] = {
    col.remove(BSONDocument("_id" -> id))
  }
}
