package services

import com.fasterxml.jackson.core.JsonParseException
import play.api.Logger
import play.api.i18n.Lang
import play.api.libs.json.{ JsObject, JsValue, Json }

import scala.annotation.tailrec
import scala.annotation.tailrec
import scala.io.Source

/**
 * User: Björn Reimer
 * Date: 03.09.14
 * Time: 12:15
 */
object LocalizationMessages {

  // parse language files
  private val messageFolder = "conf/messages/"
  private val defaultLanguage: Lang = Lang("en-Us")

  private val messages: Map[Lang, JsObject] = new java.io.File(messageFolder).listFiles.toSeq.foldLeft[Map[Lang, JsObject]](Map()) {
    case (map, file) =>
      Logger.info("Parsing language file: " + file.getAbsolutePath)
      try {
        // todo: parsing like takes a lot of memory for large files, use streaming
        val json: JsObject = Json.parse(Source.fromFile(file.getAbsolutePath).getLines().mkString).as[JsObject]
        map + (Lang(file.getName.split('.')(0)) -> json)
      } catch {
        case e: JsonParseException =>
          Logger.error("Could not parse language file: " + file.getAbsolutePath, e)
          map
      }
  }

  def get(key: String, language: Lang, variables: Map[String, String] = Map()): String = {

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

    val withVariables = messages.get(language) match {
      case Some(json) => getValue(json, key)
      case None       => get(key, defaultLanguage, variables)
    }

    // replace variables
    @tailrec
    def replaceVariables(message: String): String = {
      message.indexOf("{{") match {
        case -1 => message
        case startIndex =>
          message.indexOf("}}") match {
            case endIndex if endIndex < startIndex =>
              Logger.error("Invalid position of }}")
              message
            case endIndex =>
              val variableName = message.substring(startIndex + 2, endIndex)
              val replaceWith = variables.getOrElse(variableName, "")
              val replaced = message.replaceAll("\\{\\{"+variableName + "\\}\\}", replaceWith)
              replaceVariables(replaced)
          }
      }
    }

    replaceVariables(withVariables)
  }

}
