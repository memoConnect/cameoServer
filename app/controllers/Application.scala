package controllers

import play.api.mvc._

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("KolibriNet"))
  }

  def staticAssets(path: String, file: String, foo: String) =
    controllers.Assets.at(path, file)

}