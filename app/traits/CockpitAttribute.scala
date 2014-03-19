package traits

import play.api.libs.json._

trait CockpitAttribute {

  def getTypeName: String
  def getIsEditable: Boolean
  def getShowInList: Boolean
  def getName: String
  def getDisplayName: String
  def getData(js: JsObject): Option[JsValue]
  def getListString(js: JsObject): Option[String]
  def transform(newData: JsObject): Reads[JsObject]

  def getEditJson(js: JsObject): Option[JsObject] = {
    getData(js).map {
      data =>
        Json.obj("name" -> getName) ++
          Json.obj("displayName" -> getDisplayName) ++
          Json.obj("isEditable" -> getIsEditable) ++
          Json.obj("data" -> data) ++
          Json.obj("type" -> getTypeName)

    }
  }

}