package models.cockpit.attributes

import play.api.libs.json._
import play.api.Logger
import traits.CockpitAttribute
import models.VerifiedString

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

  def transform(newJs: JsObject): Reads[JsObject] = {
    (newJs \ name).asOpt[JsValue] match {
      case None                    => __.json.pickBranch
      case Some(js) if !isEditable => __.json.pickBranch
      case Some(js) =>
        js.asOpt[String] match {
          case None =>
            Logger.error("Cannot be converted back to type: " + js)
            __.json.pickBranch
          case Some(str) =>
            val vs = VerifiedString.create(str)
            __.json.update((__ \ name).json.put(Json.toJson(vs)))
        }
    }
  }
}
