package models.cockpit.attributes

import models.VerifiedString
import play.api.Logger
import play.api.libs.json._
import traits.CockpitAttribute

case class CockpitAttributeVerifiedString(name: String,
                                          displayName: String,
                                          isEditable: Boolean = false,
                                          showInList: Boolean = false)(implicit val format: Format[VerifiedString]) extends CockpitAttribute {

  def getTypeName = "string"
  def getShowInList = showInList
  def getIsEditable = isEditable
  def getName = name
  def getDisplayName = displayName

  def getData(js: JsObject): Option[JsValue] = {
    (js \ name).asOpt[JsValue] match {
      case None => None
      case Some(attributeJs) =>
        attributeJs.asOpt[VerifiedString] match {
          case None =>
            Logger.error("AttributeDoes not match specified type: " + js)
            None
          case Some(vs) => Some(JsString(vs.value))
        }
    }
  }

  def getListString(js: JsObject): Option[String] = {
    showInList match {
      case false => None
      case true  => getData(js).map(_.toString())
    }
  }

  def getTransformerFromData(data: JsValue): Option[Reads[JsObject]] = {
    val value: Option[JsValue] = data match {
      case JsNull => Some(JsNull)
      case JsString(str) =>
        val vs = VerifiedString.create(str)
        Some(Json.toJson(vs))
      case _ =>
        Logger.error("Cannot be converted to verified string: " + data)
        None
    }
    value.map {
      js =>
        __.json.update((__ \ name).json.put(js))
    }
  }

}
