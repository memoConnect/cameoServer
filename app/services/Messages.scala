package services

import play.api.Logger
import play.api.i18n.Lang
import play.api.libs.json.{JsObject, JsValue, Json}

import scala.io.Source

/**
 * User: Björn Reimer
 * Date: 03.09.14
 * Time: 12:15
 */
object Messages {

  // parse language files
  private val messageFolder = "conf/messages/"
  private val defaultLanguage: Lang = Lang("en-Us")

  private val messages: Map[Lang, JsObject] = new java.io.File(messageFolder).listFiles.toSeq.foldLeft[Map[Lang, JsObject]](Map()) {
    case (map, file) =>
      Logger.info("Parsing language file: " + file.getAbsolutePath)
      // todo: add error handling for invalid files
      map + (Lang(file.getName.split('.')(0)) -> Json.parse(Source.fromFile(file.getAbsolutePath).getLines().mkString).as[JsObject])

  }

  def get(key: String, language: Lang): String = {
    messages.get(language) match {
      case Some(json) => (json \ key).asOpt[String].getOrElse(key)
      case None => get(key, defaultLanguage)
    }
  }

}
