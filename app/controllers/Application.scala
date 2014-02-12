package controllers

import play.api.mvc._
import services.Authentication._
import play.api.libs.concurrent.Execution.Implicits._
import helper.MongoHelper._
import play.modules.reactivemongo.json.collection.JSONCollection
import java.io.FileWriter
import play.api.libs.json.JsObject
import services.DbAdminUtilities

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