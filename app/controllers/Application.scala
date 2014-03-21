package controllers

import play.api.mvc._
import helper.DbAdminUtilities
import play.api.Logger

object Application extends Controller {

  def redirect(url: String) = Action {
    Redirect(url)
  }

  def index = Action {
    request =>
      Ok(views.html.index())
  }

  def dumpDb() = Action {

    DbAdminUtilities.dumpDb()
    Ok("dumped")

  }

  def loadFixtures = Action {
    DbAdminUtilities.loadFixtures()
    Ok("loaded")
  }

  def staticAssets(path: String, file: String, foo: String) =
    controllers.Assets.at(path, file)

}