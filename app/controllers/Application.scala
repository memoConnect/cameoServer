package controllers

import play.api.mvc._
import services.Authentication._
import play.api.libs.concurrent.Execution.Implicits._

object Application extends Controller {

//  def index = Action {
//    Ok(views.html.index("KolibriNet"))
//  }

//  def index = Authenticated { request =>
//    Ok("hello, " + request.token)
//  }

  def index = Action {
    request =>

      val cc: UserClass = getUserClass("anon")

    Ok("hello, " + cc)
  }

  def staticAssets(path: String, file: String, foo: String) =
    controllers.Assets.at(path, file)

}