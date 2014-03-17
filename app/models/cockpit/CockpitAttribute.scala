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
    cockpitAttribute => toJson ++ Json.obj(typeKey -> attributeType)
  }

  def toJson: JsObject
}