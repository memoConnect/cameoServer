package models.cockpit.attributes

import traits.CockpitAttribute
import play.api.libs.json._
import play.api.Logger
import play.api.libs.json.JsObject
import scala.Some

case class CockpitAttributeOptionList[A](name: String,
                                         displayName: String,
                                         options: Seq[A],
                                         isEditable: Boolean = false,
                                         showInList: Boolean = false)(implicit val format: Format[A]) extends CockpitAttribute {

  def getTypeName = "optionList"
  def getShowInList = showInList
  def getIsEditable = isEditable
  def getName = name
  def getDisplayName = displayName

  def getData(js: JsObject): Option[JsValue] = {
    (js \ name).asOpt[JsValue] match {
      case None => None
      case Some(attributeJs) =>
        attributeJs.asOpt[A] match {
          case None =>
            Logger.error("AttributeDoes not match specified type: " + js)
            None
          case Some(obj) =>
            val optionsJs = options.map(Json.toJson(_))
            val selected = Json.toJson(obj)
            Some(Json.obj("options" -> optionsJs, "selected" -> selected))
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
    data.asOpt[A] match {
      case None =>
        Logger.error("Cannot be converted back to type: " + data)
        None
      case Some(obj) =>
        Some(__.json.update((__ \ name).json.put(Json.toJson(obj))))
    }
  }
}
