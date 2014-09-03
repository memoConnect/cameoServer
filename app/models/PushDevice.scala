package models

import helper.IdHelper
import play.api.Logger
import play.api.i18n.Lang
import play.api.libs.json.{ Json, Format, JsObject, Reads }
import traits.{ Model, SubModel }
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import helper.JsonHelper._

/**
 * User: Björn Reimer
 * Date: 03.09.14
 * Time: 14:23
 */
case class PushDevice(deviceId: MongoId,
                      platform: String,
                      language: String) {

  def toJson: JsObject = Json.obj(
    "deviceId" -> deviceId.toJson,
    "platform" -> platform,
    "language" -> language
  )
}

object PushDevice extends SubModel[PushDevice, Account] {
  def parentModel = Account

  def elementName = "pushDevices"

  override val idName = "deviceId"

  implicit def mongoFormat: Format[PushDevice] = createMongoFormat(Json.reads[PushDevice], Json.writes[PushDevice])

  def createReads: Reads[PushDevice] =
    ((__ \ 'deviceId).read[MongoId](MongoId.createReads) and
      (__ \ 'platform).read[String] and
      (__ \ 'language).read[String](verifyLanguageId)
    )(PushDevice.apply _)

  def createDefault(): PushDevice = new PushDevice(IdHelper.generateMongoId(), "foo", "en-US")

  def docVersion: Int = 0

  def evolutions: Map[Int, Reads[JsObject]] = Map()
}
