package controllers

import play.api.mvc._
import play.api.{Logger, Play}
import play.api.Play.current

object Application extends Controller {

  def index = Action {

    Logger.debug(Play.configuration.getString("mongodb.uri").getOrElse("Moo"))

    Ok(views.html.index("KolibriNet"))
  }

}