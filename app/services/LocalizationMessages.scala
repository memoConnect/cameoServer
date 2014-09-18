package services

import com.fasterxml.jackson.core.JsonParseException
import play.api.Logger
import play.api.i18n.Lang
import play.api.libs.json.{ JsObject, Json }

import scala.annotation.tailrec
import scala.io.Source

/**
 * User: Björn Reimer
 * Date: 03.09.14
 * Time: 12:15
 */

//todo: improve handling of same languages from different countries. eg: en_UK and en_US
object LocalizationMessages {

  // parse language files
  private val messageFolder = "conf/messages/"
  val defaultLanguage: Lang = Lang("en-Us")

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

  @tailrec
  private def getKeyFromJson(json: JsObject, key: String, originalKey: String): String = {

    // first try to get the key
    (json \ key).asOpt[String] match {
      case Some(value) => value
      case None =>
        // try to get subkey
        key.split('.').toList match {
          case Nil =>
            key
          case value :: Nil =>
            (json \ value).asOpt[String].getOrElse(originalKey)
          case value :: rest =>
            val reduced = (json \ value).asOpt[JsObject].getOrElse(Json.obj())
            getKeyFromJson(reduced, rest.mkString("."), originalKey)
        }
    }
  }

  // replace variables
  @tailrec
  def replaceVariables(message: String, variables: Map[String, String]): String = {
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
            val replaced = message.replaceAll("\\{\\{" + variableName + "\\}\\}", replaceWith)
            replaceVariables(replaced, variables)
        }
    }
  }

  def get(key: String, language: Lang, variables: Map[String, String] = Map()): String = {
    val withVariables = messages.get(language) match {
      case Some(json) => getKeyFromJson(json, key, key)
      case None       => get(key, defaultLanguage, variables)
    }

    replaceVariables(withVariables, variables)
  }

  def getAll(key: String, variables: Map[String, String] = Map()): Map[Lang, String] = {
    messages.mapValues {
      json => replaceVariables(getKeyFromJson(json, key, key), variables)
    }
  }

}
