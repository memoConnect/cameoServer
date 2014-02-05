package controllers

import play.api.mvc._
import services.Authentication._
import play.api.libs.concurrent.Execution.Implicits._

object Application extends Controller {

  def redirect(url: String) = Action {
    Redirect(url)
  }

  def index = Action {
    request =>

      Ok("Cameo API")
  }

  def staticAssets(path: String, file: String, foo: String) =
    controllers.Assets.at(path, file)

}