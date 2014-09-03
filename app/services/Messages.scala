package services

import play.api.Logger
import play.api.i18n.Lang
import play.api.libs.json.{JsObject, JsValue, Json}

import scala.annotation.tailrec
import scala.annotation.tailrec
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

  @tailrec
  def get(key: String, language: Lang): String = {

    @tailrec
    def getValue(json: JsObject, key: String): String = {
      key.split('.').toList match {
        case Nil =>
          key
        case value :: Nil =>
          (json \ value).asOpt[String].getOrElse(key)
        case value :: rest =>
          val reduced = (json \ value).asOpt[JsObject].getOrElse(Json.obj())
          getValue(reduced, rest.mkString("."))
      }
    }

    messages.get(language) match {
      case Some(json) => getValue(json, key)
      case None => get(key, defaultLanguage)
    }
  }

}
