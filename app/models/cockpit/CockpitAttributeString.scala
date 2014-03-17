package models.cockpit

import play.api.libs.json.{JsValue, Writes, Json, JsObject}

/**
 * Created by dermicha on 17.03.14.
 */
case class CockpitAttributeString(name: String,
                                  isEditable: Boolean,
                                  data: Option[String]
                                   ) extends CockpitAttribute {
  val attributeType: String = "String"

  def toJson: JsObject = {
    Json.obj(nameKey -> name) ++
      Json.obj(isEditableKey -> isEditable) ++
      Json.obj(dataKey -> data)
  }
}
