package helper

import play.api.libs.json._
import play.api.libs.json.Reads._
import reactivemongo.bson.BSONObjectID
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.JsNumber
import play.api.libs.functional.syntax._


/**
 * User: Björn Reimer
 * Date: 5/21/13
 * Time: 6:53 PM
 */

/**
 * Several Helper functions for interaction with MongoDB *
 */

trait MongoHelper {

  // empty Object
  val emptyObj = __.json.put(Json.obj())

  /// Generate Object ID and creation date
  val generateId = (__ \ '_id \ '$oid).json.put(JsString(BSONObjectID.generate.stringify))
  val generateCreated = (__ \ 'created \ '$date).json.put(JsNumber((new java.util.Date).getTime))

  val addObjectId: Reads[JsObject] = __.json.update(generateId)
  val addCreateDate: Reads[JsObject] = __.json.update(generateCreated)
  val addObjectIdAndDate: Reads[JsObject] = __.json.update((generateId and generateCreated).reduce)

  // generate result
  def resOK(data: JsValue) = Json.obj("res" -> "OK") ++ Json.obj("data" -> data)
  def resKO(error: JsValue) = Json.obj("res" -> "KO") ++ Json.obj("error" -> error)

  // convert object id and date between json and bson format
  val toObjectId = Writes[String] {
    s => Json.obj("_id" -> Json.obj("$oid" -> s))
  }
  val fromObjectId = (__ \ 'id).json.copyFrom((__ \ '_id \ '$oid).json.pick)
  val fromCreated = __.json.update((__ \ 'created).json.copyFrom((__ \ 'created \ '$date).json.pick))

  // add status message
  def addStatus(status: String): Reads[JsObject] = __.json.update((__ \ 'status).json.put(JsString(status)))


}
