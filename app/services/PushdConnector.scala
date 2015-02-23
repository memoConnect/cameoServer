package services

import play.api.Play.current
import play.api.i18n.Lang
import play.api.libs.json._
import play.api.libs.ws.{ WS, WSResponse }
import play.api.{ Logger, Play }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 17.09.14
 * Time: 14:10
 */
object PushdConnector {

  val maybeUrl = Play.configuration.getString("pushd.url")

  // available platforms
  trait PushdPlatform
  case object GCM extends PushdPlatform
  case object APNS extends PushdPlatform
  case object MPNS extends PushdPlatform

  implicit val platformReads: Reads[PushdPlatform] = Reads {
    js =>
      js.asOpt[String] match {
        case Some("ios") => JsSuccess(APNS)
        case Some("and") => JsSuccess(GCM)
        case Some("win") => JsSuccess(MPNS)
        case _           => JsError("invalid platform")
      }
  }

  def getSubscriberId(token: String, platform: PushdPlatform, language: Lang): Future[Option[String]] = {

    val proto = platform match {
      case GCM  => "gcm"
      case APNS => "apns"
      case MPNS => "mpns"
    }

    val body: Map[String, String] = Map(
      "proto" -> proto,
      "token" -> token,
      "lang" -> language.code
    )

    postRequest("/subscribers", body).map {
      _.map {
        response => (response.json \ "id").asOpt[String]
      }.recover {
        case e: Exception =>
          if (!Play.isTest) Logger.error("Could not connect to pushd", e)
          None
      }
    }.getOrElse {
      if (!Play.isTest) Logger.warn("pushd not configured")
      Future(None)
    }

  }

  def setSubscriptions(subscriberId: String, eventIds: Seq[String]): Future[Boolean] = {
    val body = eventIds.map(e => e -> Json.obj("ignore_message" -> false)).toMap

    postRequest("/subscriber/" + subscriberId + "/subscriptions", Json.toJson(body)).map {
      _.map {
        response => response.status < 400
      }.recover {
        case e: Exception =>
          if (!Play.isTest) Logger.error("Could not connect to pushd", e)
          false
      }
    }.getOrElse {
      if (!Play.isTest) Logger.warn("pushd not configured")
      Future(false)
    }
  }

  def sendEvent(eventId: String, titles: Map[Lang, String], content: Map[Lang, String], context: String): Future[Boolean] = {

    val body: Map[String, String] = Map(
      "title" -> titles.get(LocalizationMessages.defaultLanguage).get,
      "msg" -> content.get(LocalizationMessages.defaultLanguage).get,
      "sound" -> context,
      "data.context" -> context
    ) ++ content.map {
        case (lang, msg) => "msg." + lang.code -> msg
      } ++ titles.map {
        case (lang, title) => "title." + lang.code -> title
      }

    postRequest("/event/" + eventId, body).map {
      _.map {
        response => response.status == 204
      }.recover {
        case e: Exception =>
          if (!Play.isTest) Logger.error("Could not connect to pushd", e)
          false
      }
    }.getOrElse {
      if (!Play.isTest) Logger.warn("pushd not configured")
      Future(false)
    }
  }

  def postRequest(path: String, body: JsValue): Option[Future[WSResponse]] = {
    maybeUrl.map {
      url =>
        WS.url(url + path).post(body)
    }
  }

  def postRequest(path: String, body: Map[String, String]): Option[Future[WSResponse]] = {
    val bodyWithSeq = body.map {
      case (key, value) => key -> Seq(value)
    }
    maybeUrl.map {
      url =>
        WS.url(url + path).post(bodyWithSeq)
    }
  }

}
