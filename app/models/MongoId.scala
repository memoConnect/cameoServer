package models

import play.api.libs.json.Reads._
import play.api.libs.json._

/**
 * User: Björn Reimer
 * Date: 1/16/14
 * Time: 4:28 PM
 */

case class MongoId(id: String) {

  override def toString = id

  def toJson: JsString = {
    JsString(id)
  }
}

object MongoId {

  def createReads: Reads[MongoId] = {
    __.read[String].map {
      l => MongoId(l)
    }
  }

  implicit def mongoReads: Reads[MongoId] =
    (__ \ 'mongoId).read[String].map {
      l => MongoId(l)
    }

  implicit def mongoWrites: Writes[MongoId] = Writes {
    id => Json.obj("mongoId" -> id.id)
  }
}
