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

  val attributesWrites: Writes[CockpitAttributeString] = Writes[CockpitAttributeString] {
    cockpitAttributeString =>
      Json.obj(cockpitAttributeString.nameKey -> cockpitAttributeString.name) ++
        Json.obj(cockpitAttributeString.isEditableKey -> cockpitAttributeString.isEditable) ++
        Json.obj(cockpitAttributeString.dataKey -> cockpitAttributeString.data)
  }


}
