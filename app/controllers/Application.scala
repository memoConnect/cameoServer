package controllers

import play.api.mvc._
import helper.Authenticated

object Application extends Controller {

//  def index = Action {
//    Ok(views.html.index("KolibriNet"))
//  }

  def index = Authenticated { request =>
    Ok("hello, " + request.token)
  }

  def staticAssets(path: String, file: String, foo: String) =
    controllers.Assets.at(path, file)

}