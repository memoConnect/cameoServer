package controllers

import net.sf.uadetector.service.UADetectorServiceFactory
import play.Logger
import play.api.Play
import play.api.Play.current
import play.api.mvc._

/**
 * Created by dermicha on 17/09/14.
 */
object AppRedirector extends Controller {

  val iosUrl = Play.configuration.getString("apr.ios").get
  val androidUrl = Play.configuration.getString("apr.android").get
  val mobileWebUrl = Play.configuration.getString("apr.mobileweb").get

  val uaParser = UADetectorServiceFactory.getResourceModuleParser()

  def redirectToApp() = Action { request =>
    request.headers.get("User-Agent") match {
      case Some(userAgent) =>

        val parsedUserAgent = uaParser.parse(userAgent)
        Logger.debug("agent: " + parsedUserAgent.getOperatingSystem)
        parsedUserAgent.getOperatingSystem.getFamilyName match {
          case "iOS" =>
            Logger.debug("iPhone found")
            Redirect(iosUrl, TEMPORARY_REDIRECT)
          case "Android" =>
            Logger.debug("Android found")
            Redirect(androidUrl, TEMPORARY_REDIRECT)
          case _ =>
            Logger.debug("all the rest found")
            Redirect(mobileWebUrl, TEMPORARY_REDIRECT)
        }
      case None =>
        Redirect("https://www.cameoNet.de", TEMPORARY_REDIRECT)
    }
  }

}
