package models.cockpit

import play.api.libs.json.{Writes, Json, JsObject, JsValue}

/**
 * Created by dermicha on 17.03.14.
 */
trait CockpitAttribute {
  val attributeType: String

  val nameKey = "attributeName"
  val typeKey = "attributeType"
  val isEditableKey = "attributeIsEditable"
  val dataKey = "attributeData"

  implicit val writes: Writes[CockpitAttribute] = Writes[CockpitAttribute] {
    cockpitAttribute => Json.toJson(this)(attributesWrites).as[JsObject] ++ Json.obj(typeKey -> attributeType)
  }

  val attributesWrites: Writes[CockpitAttribute]

  def toJson: JsObject = {
    Json.toJson(this).as[JsObject]
  }
}