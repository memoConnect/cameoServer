package controllers

import play.api.mvc._
import play.api.{Logger, Play}
import play.api.Play.current

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("KolibriNet"))
  }

}