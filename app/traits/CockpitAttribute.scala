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

  def getTransformerFromData(data: JsValue): Option[Reads[JsObject]]

  def getTransformer(newJs: JsObject): Option[Reads[JsObject]] = {
    (newJs \ getName).asOpt[JsValue] match {
      case None                         => None
      case Some(data) if !getIsEditable => None
      case Some(data)                   => getTransformerFromData(data)
    }
  }

  def getEditJson(js: JsObject): JsObject = {

    val data = getData(js)
    val dataOrNull = data.getOrElse(JsNull)
    Json.obj("name" -> getName) ++
      Json.obj("displayName" -> getDisplayName) ++
      Json.obj("isEditable" -> getIsEditable) ++
      Json.obj("data" -> dataOrNull) ++
      Json.obj("type" -> getTypeName)
  }

}