package models.cockpit.attributes

import play.api.libs.json._
import play.api.Logger
import traits.CockpitAttribute
import java.util.Date

case class CockpitAttributeDate(name: String,
                                displayName: String,
                                isEditable: Boolean = false,
                                showInList: Boolean = false) extends CockpitAttribute {

  def getTypeName = "string"
  def getShowInList = showInList
  def getIsEditable = isEditable
  def getName = name
  def getDisplayName = displayName

  def getData(js: JsObject): Option[JsValue] = {
    (js \ name \ "$date").asOpt[Long].map {
      num =>
        val date = new Date(num)
        JsString(date.toString)
    }
  }

  def getListString(js: JsObject): Option[String] = {
    showInList match {
      case false => None
      case true  => getData(js).map(_.toString())
    }
  }

  def getTransformerFromData(data: JsValue): Option[Reads[JsObject]] = {
    data.asOpt[JsNumber] match {
      case None =>
        Logger.error("Cannot be converted back to type: " + data)
        None
      case Some(obj) =>
        Some(__.json.update((__ \ name).json.put(Json.toJson(obj))))
    }
  }
}
