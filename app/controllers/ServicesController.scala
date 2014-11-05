package controllers

import helper.ResultHelper._
import helper.Utils.InvalidVersionException
import helper.{UserAgentHelper, HTTPHelper, CheckHelper, Utils}
import play.Logger
import play.api.Play
import play.api.Play.current
import play.api.libs.json.{ JsValue, Json }
import play.api.mvc.Action
import play.modules.statsd.api.Statsd
import traits.ExtendedController

import scala.collection.immutable.HashMap

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
          case None    => resKo("invalid phone number:" + phoneNumber)
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
          case None    => resKo("invalid emailAddress: " + email)
          case Some(e) => resOk(Json.obj("email" -> e))
        }
        case None => resBadRequest("missing emailAddress")
      }
  }

  case class CheckMixedField(mixed: String)
  object CheckMixedField { implicit val format = Json.format[CheckMixedField] }
  def checkMixedField = Action(parse.tolerantJson) {
    request =>
      validate(request.body, CheckMixedField.format) {
        cmf =>
          CheckHelper.checkAndCleanMixed(cmf.mixed) match {
            case Some(Left(tel))    => resOk(Json.obj("phoneNumber" -> tel))
            case Some(Right(email)) => resOk(Json.obj("email" -> email))
            case None               => resKo("Neither phonenumber nor email: " + cmf.mixed)
          }
      }
  }

  case class GetBrowserInfo(version: String)
  object GetBrowserInfo { implicit val format = Json.format[GetBrowserInfo] }

  case class GetBrowserInfoResponse(languageCode: String,
                                    versionIsSupported: Boolean)
  object GetBrowserInfoResponse { implicit val format = Json.format[GetBrowserInfoResponse] }

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
      resOk(Json.toJson(GetBrowserInfoResponse(language, versionIsSupported = true)))
  }

  val iosUrl = Play.configuration.getString("app.download.ios").get
  val androidUrl = Play.configuration.getString("app.download.android").get
  val defaultUrl = Play.configuration.getString("app.download.default").get

  val iosOpf: String = UserAgentHelper.osFamilyIos
  val androidOpf: String = UserAgentHelper.osFamilyAndroid

  val osUrlMapping = HashMap(
    UserAgentHelper.osFamilyIos -> iosUrl,
    UserAgentHelper.osFamilyAndroid -> androidUrl
  )

  //@TODO add WindowsPhone handling
  def redirectToApp() = Action { request =>
    request.headers.get("User-Agent") match {
      case Some(userAgent) =>
        val parsedUserAgent = HTTPHelper.parseUserAgent(userAgent)
        val currentOsf = parsedUserAgent.getFamilyName
        Logger.debug("%s found".format(currentOsf))
        val targetUrl = osUrlMapping.getOrElse(currentOsf, defaultUrl)
        Logger.debug("current target URL %s".format(targetUrl))
        Redirect(targetUrl, TEMPORARY_REDIRECT)
      case None =>
        Redirect(defaultUrl, TEMPORARY_REDIRECT)
    }
  }
}
