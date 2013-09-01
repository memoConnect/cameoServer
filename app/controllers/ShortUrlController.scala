package controllers

import play.api.mvc.{Action, Controller}
import play.api.Play
import play.api.Play.current

/**
 * User: Björn Reimer
 * Date: 6/17/13
 * Time: 4:01 PM
 */
object ShortUrlController extends Controller {

  def redirect(urlType: String, id: String) = Action {
    request =>
      val redirectTo = Play.configuration.getString("shortUrl.redirectTo").getOrElse("derp")

      urlType match {
        case "c" => Redirect(redirectTo + "/conversation/" + id)
        case "p" => Redirect(redirectTo + "/purl/" + id)
        case _ => Redirect(redirectTo)
      }
  }
}
