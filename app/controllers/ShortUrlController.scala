package controllers

import play.api.mvc.{Action, Controller}
import play.api.Play
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import helper.ResultHelper._


/**
 * User: Björn Reimer
 * Date: 6/17/13
 * Time: 4:01 PM
 */
object ShortUrlController extends Controller {

  def redirect(urlType: String, id: String) = Action {
    request =>
      val redirect = Play.configuration.getString("shortUrl.redirectTo").get

      urlType match {
        case "p" => Redirect(redirect + "/purl/" + id)
        case "v" => Redirect(redirect + "/verification/" + id)
        case _ => Redirect(redirect)
      }
  }
}
