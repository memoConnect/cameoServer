package services

import com.fasterxml.jackson.core.JsonParseException
import play.api.Play.current
import play.api.i18n.Lang
import play.api.libs.json.{ JsObject, Json }
import play.api.libs.ws.WS
import play.api.mvc.Request
import play.api.{ Logger, Play }
import scala.concurrent.duration._
import scala.annotation.tailrec
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 03.09.14
 * Time: 12:15
 */

//todo: improve handling of same languages from different countries. eg: en_UK and en_US
object LocalizationMessages {

  // parse language files
  private val messagesPath = Play.configuration.getString("language.messages.path").getOrElse("")
  val defaultLanguage: Lang = Lang(Play.configuration.getString("language.default").getOrElse("en"))
  val supportedLanguages: Seq[Lang] = Play.configuration.getStringSeq("language.supported").getOrElse(Seq()).map(Lang(_))

  private val messages: Map[Lang, JsObject] = supportedLanguages.foldLeft[Map[Lang, JsObject]](Map()) {
    case (map, lang) =>
      val path = messagesPath + "/" + lang.code + ".json"
      val res = Play.resource(path)
      Logger.info("Getting messages from: " + path)
      try {
        // todo parse json directly from stream
        val json = Json.parse(scala.io.Source.fromInputStream(res.get.openStream()).mkString).as[JsObject]
        map + (lang -> json)
      } catch {
        case e: JsonParseException =>
          Logger.error("Could not parse json from  " + path, e)
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

    val withVariables = messages.find(l => language.satisfies(l._1)) match {
      case Some((l, json)) => getKeyFromJson(json, key, key)
      case None            => get(key, defaultLanguage, variables)
    }

    replaceVariables(withVariables, variables)
  }

  def getAll(key: String, variables: Map[String, String] = Map()): Map[Lang, String] = {
    messages.mapValues {
      json => replaceVariables(getKeyFromJson(json, key, key), variables)
    }
  }

  def getBrowserLanguage[A](request: Request[A]): Lang = {
    request.acceptLanguages.headOption.getOrElse {
      Lang(Play.configuration.getString("language.default").getOrElse("en"))
    }
  }

}
