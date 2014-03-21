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
                                  filterName: String) extends CockpitAttribute {

  def getTypeName = "filter"

  def getShowInList = false

  def getIsEditable = false

  def getName = name

  def getDisplayName = displayName

  def getData(js: JsObject): Option[JsValue] = {
    val res = Json.obj(
      "filterName" -> filterName,
      "listName" -> listName)

    (js \ name).asOpt[MongoId] match {
      case Some(id) => Some(res ++ Json.obj("filterTerm" -> id.id))
      case None =>
        (js \ name).asOpt[Seq[MongoId]] match {
          case Some(list) =>
            val strings = list.map(_.id)
            Some(res ++ Json.obj("filterTerm" -> strings.mkString("(", "|", ")")))
          case None =>
            (js \ name).asOpt[Seq[JsObject]] match {
              case Some(list2) =>
                // special hack for contacts todo: make this generic
                list2.headOption.flatMap {
                  head =>
                    (head \ "identityId").asOpt[MongoId].isDefined match {
                      case false => None
                      case true =>
                        val strings = list2.map(js => (js \ "identityId").as[MongoId].id)
                        Some(res ++ Json.obj("filterTerm" -> strings.mkString("(", "|", ")")))
                    }
                }
              case None => None
            }
        }
    }
  }

  def getListString(js: JsObject): Option[String] = None

  def getTransformerFromData(data: JsValue): Option[Reads[JsObject]] = None
}
