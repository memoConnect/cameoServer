package models

import reactivemongo.bson.BSONObjectID

import play.api.libs.json._
import play.api.libs.json.Reads._

/**
 * User: Björn Reimer
 * Date: 1/16/14
 * Time: 4:28 PM
 */

case class MongoId(id: String) {

  override def toString = id
}

object MongoId {

  def create(): MongoId = {
    new MongoId(BSONObjectID.generate.stringify)
  }

  implicit def mongoReads: Reads[MongoId] =
    (__ \ '$oid).read[String].map {
      l => MongoId(l)
    }

  implicit def mongoWrites: Writes[MongoId] = Writes {
    id => Json.obj("$oid" -> id.id)
  }
}
