package models

import helper.MongoCollections._
import reactivemongo.bson.{BSONArray, BSONBinary, BSONDocument, Subtype}
import reactivemongo.core.commands.LastError

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 1/15/14
 * Time: 11:59 AM
 */
object FileChunk {

  def col = fileChunkCollection

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

  def deleteAll(ids: Seq[String]):Future[LastError] = {
    val idDocuments = ids.map(id => BSONDocument("_id" -> id))
    col.remove(BSONDocument("$or" -> BSONArray(idDocuments)))
  }
}
