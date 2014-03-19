package models.cockpit

import play.api.libs.json.{ JsString, Json, Writes, JsObject }

/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 11:59 AM
 */

case class CockpitEdit(id: String,
                       attributes: Seq[JsObject]) {
  def toJson: JsObject = Json.toJson(this).as[JsObject]
}

object CockpitEdit {
  implicit val writes: Writes[CockpitEdit] = Json.writes[CockpitEdit]
}