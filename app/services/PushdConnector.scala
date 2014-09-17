package services

import play.api.i18n.Lang
import play.api.libs.json.{ JsValue, Json }
import play.api.{ Logger, Play }
import play.api.Play.current
import play.api.http.{ ContentTypeOf, Writeable }
import play.api.libs.ws.{ WSResponse, WSRequestHolder, WS }
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 17.09.14
 * Time: 14:10
 */
object PushdConnector {

  val url = Play.configuration.getString("pushd.url").getOrElse("http://localhost")

  // available platforms
  trait PushdPlatform
  case object GCM extends PushdPlatform
  case object APNS extends PushdPlatform
  case object MPNS extends PushdPlatform

  def getSubscriberId(token: String, platform: PushdPlatform, language: Lang): Future[Option[String]] = {

    val proto = platform match {
      case GCM  => "gcm"
      case APNS => "apns"
      case MPNS => "mpns"
    }

    val body: Map[String, Seq[String]] = Map(
      "proto" -> Seq(proto),
      "token" -> Seq(token),
      "lang" -> Seq(language.code)
    )

    postRequest("/subscribers", body).map {
      response =>
        (response.json \ "id").asOpt[String]
    }.recover {
      case e: Exception =>
        Logger.error("Could not connect to pushd", e)
        None
    }
  }

  def setSubscriptions(subscriberId: String, eventNames: Seq[String]): Future[Boolean] = {
    val body = eventNames.map(e => e -> Json.obj("ignore_message" -> false)).toMap

    postRequest("/subscriber/" + subscriberId + "/subscriptions", Json.toJson(body)).map {
      response =>
        response.status < 400
    }.recover {
      case e: Exception =>
        Logger.error("Could not connect to pushd", e)
        false
    }
  }

  def postRequest(path: String, body: JsValue): Future[WSResponse] = {
    // intellij does not like this for some reason, but it compiles...
    WS.url(url + path).post(body)
  }

  def postRequest(path: String, body: Map[String, Seq[String]]): Future[WSResponse] = {
    // intellij does not like this for some reason, but it compiles...
    WS.url(url + path).post(body)
  }

}
