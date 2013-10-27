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
      val prodRedirect = Play.configuration.getString("shortUrl.redirectTo").getOrElse("derp")
      val stageRedirect = Play.configuration.getString("shortUrl.redirectTo").getOrElse("derp")

      urlType match {
        case "p" => Redirect(prodRedirect + "/purl/" + id)
        case "s" => Redirect(stageRedirect + "/purl/" + id)
        case _ => Redirect(prodRedirect)
      }
  }
}
