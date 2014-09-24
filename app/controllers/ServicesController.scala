package controllers

import helper.ResultHelper._
import helper.Utils.InvalidVersionException
import helper.{ CheckHelper, Utils }
import net.sf.uadetector.OperatingSystemFamily
import net.sf.uadetector.service.UADetectorServiceFactory
import play.api.Play.current
import play.api.libs.json.{ JsValue, Json }
import play.api.mvc.Action
import play.api.{ Logger, Play }
import play.modules.statsd.api.Statsd
import traits.ExtendedController

import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
 * User: Michael Merz
 * Date: 31/01/14
 * Time: 4:30 PM
 */

object ServicesController extends ExtendedController {

  def checkPhoneNumber = Action(parse.tolerantJson) {
    request =>
      val jsBody: JsValue = request.body
      (jsBody \ "phoneNumber").asOpt[String] match {
        case Some(phoneNumber) => CheckHelper.checkAndCleanPhoneNumber(phoneNumber) match {
          case None    => resBadRequest("invalid phone number")
          case Some(p) => resOk(Json.obj("phoneNumber" -> p))
        }
        case None => resBadRequest("no phoneNumber")
      }
  }

  def checkEmailAddress = Action(parse.tolerantJson) {
    request =>
      val jsBody: JsValue = request.body
      (jsBody \ "emailAddress").asOpt[String] match {
        case Some(email) => CheckHelper.checkAndCleanEmailAddress(email) match {
          case None    => resBadRequest("invalid emailAddress")
          case Some(e) => resOk(Json.obj("email" -> e))
        }
        case None => resBadRequest("missing emailAddress")
      }
  }

  case class GetBrowserInfo(version: String)

  object GetBrowserInfo {
    implicit val format = Json.format[GetBrowserInfo]
  }

  case class GetBrowserInfoResponse(languageCode: String,
                                    versionIsSupported: Boolean)

  object GetBrowserInfoResponse {
    implicit val format = Json.format[GetBrowserInfoResponse]
  }

  def getBrowserInfoPost = Action(parse.tolerantJson) {
    request =>
      validate(request.body, GetBrowserInfo.format) {
        getBrowserInfo =>

          Statsd.increment("custom.version." + getBrowserInfo.version)

          val language = request.acceptLanguages.headOption match {
            case None       => Play.configuration.getString("language.default").getOrElse("en-US")
            case Some(lang) => lang.code
          }

          val supportedVersion = Play.configuration.getString("client.version.min").getOrElse("0")
          try {
            val supported = Utils.compareVersions(supportedVersion, getBrowserInfo.version)
            val res = GetBrowserInfoResponse(language, supported)
            resOk(Json.toJson(res))
          } catch {
            case InvalidVersionException(msg) => resBadRequest("Invalid version: " + msg)
          }
      }
  }

  def getBrowserInfoGet = Action {
    request =>
      val language = request.acceptLanguages.headOption match {
        case None       => Play.configuration.getString("language.default").getOrElse("enUS")
        case Some(lang) => lang.code
      }
      resOk(Json.toJson(GetBrowserInfoResponse(language, true)))
  }

  val iosUrl = Play.configuration.getString("app.download.ios").get
  val androidUrl = Play.configuration.getString("app.download.android").get
  val defaultUrl = Play.configuration.getString("app.download.default").get

  val uaParser = UADetectorServiceFactory.getResourceModuleParser

  val iosOpf: String = OperatingSystemFamily.IOS.getName
  val androidOpf: String = OperatingSystemFamily.ANDROID.getName

  val osUrlMapping = HashMap(
    OperatingSystemFamily.IOS.getName -> iosUrl,
    OperatingSystemFamily.ANDROID.getName -> androidUrl
  )
  //.withDefaultValue(defaultUrl)

  //@TODO add WindowsPhone handling
  def redirectToApp() = Action { request =>
    request.headers.get("User-Agent") match {
      case Some(userAgent) =>
        val parsedUserAgent = uaParser.parse(userAgent)
        val currentOsf = parsedUserAgent.getOperatingSystem.getFamilyName
        Logger.debug("%s found".format(currentOsf))
        val targetUrl = osUrlMapping.getOrElse(currentOsf, defaultUrl)
        Logger.debug("current target URL %s".format(targetUrl))
        Redirect(targetUrl, TEMPORARY_REDIRECT)
      case None =>
        Redirect(defaultUrl, TEMPORARY_REDIRECT)
    }
  }
}
