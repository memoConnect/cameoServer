package controllers

import play.api.mvc._
import services.Authentication._
import play.api.libs.concurrent.Execution.Implicits._
import helper.JsonHelper._
import play.modules.reactivemongo.json.collection.JSONCollection
import java.io.FileWriter
import play.api.libs.json.{Json, JsObject}
import helper.DbAdminUtilities
import play.api.{Logger, Play}
import helper.ResultHelper._
import scala.concurrent.Future
import reactivemongo.bson.BSONDocument
import models.Account

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

  def checkApp = Action.async {
    Account.col.find(Json.obj()).one[Account].flatMap {
      case Some(wummel) => Future(resOK())
      case None => Future(resKO("database connection down!"))
    }
  }
}