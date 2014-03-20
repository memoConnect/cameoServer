package models.cockpit.attributes

import traits.CockpitAttribute
import play.api.libs.json._
import play.api.libs.json.JsObject
import scala.Some
import play.api.Logger
import models.MongoId

case class CockpitAttributeFilter(name: String,
                                  displayName: String,
                                  listName: String,
                                  filterName: String
                                   ) extends CockpitAttribute {

  def getTypeName = "filter"

  def getShowInList = false

  def getIsEditable = false

  def getName = name

  def getDisplayName = displayName

  def getData(js: JsObject): Option[JsValue] = {
    (js \ name).asOpt[MongoId].map {
      id =>
        Json.obj(
          "filterName" -> filterName,
          "listName" -> listName,
          "filterTerm" -> id.id)
    }
  }

  def getListString(js: JsObject): Option[String] = None

  def getTransformerFromData(data: JsValue): Option[Reads[JsObject]] = None
}
