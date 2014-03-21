package models.cockpit.attributes

import play.api.libs.json._
import traits.CockpitAttribute

case class CockpitAttributeSimpleList(name: String,
                                      displayName: String) extends CockpitAttribute {

  def getTypeName = "simpleList"
  def getShowInList = false
  def getIsEditable = false
  def getName = name
  def getDisplayName = displayName

  def getData(js: JsObject): Option[JsValue] = {
    (js \ name).asOpt[Seq[JsValue]] match {
      case None => None
      case Some(list) =>
        Some(Json.toJson(list))
    }
  }

  def getListString(js: JsObject): Option[String] = None

  def getTransformerFromData(data: JsValue): Option[Reads[JsObject]] = None
}
