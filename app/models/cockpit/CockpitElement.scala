package models.cockpit

import play.api.libs.json.{JsString, Json, Writes, JsObject}

/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 11:59 AM
 */
case class CockpitElement(id: String,
                          attributes: Seq[(String, Option[String])]) {
  def toJson: JsObject = Json.toJson(this).as[JsObject]
}

object CockpitElement {
  implicit val writes: Writes[CockpitElement] = Writes[CockpitElement] {
    cockpitElement =>

      val elements: Seq[JsObject] = cockpitElement.attributes.map {
        case (key, value) => Json.obj(key -> JsString(value.getOrElse("--")))
      }
      Json.obj("id" -> cockpitElement.id) ++
        Json.obj("attributes" -> elements)
  }
}