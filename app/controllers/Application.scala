package controllers

import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.Json
import helper.DbAdminUtilities
import play.api.Play
import helper.ResultHelper._
import models.Account
import play.api.Play.current

object Application extends Controller {

  def redirect(url: String) = Action {
    Redirect(url)
  }

  def index = Action {
    request =>
      Ok(views.html.index())
  }

  def dumpDb() = Action {
    Play.isDev match {
      case true =>
        DbAdminUtilities.dumpDb()
        Ok("dumped")
      case false =>
        resBadRequest("not in dev mode")
    }
  }

  def loadFixtures = Action {
    Play.isDev match {
      case true =>
        DbAdminUtilities.loadFixtures()
        Ok("loaded")
      case false =>
        resBadRequest("not in dev mode")
    }
  }

  def staticAssets(path: String, file: String, foo: String) =
    controllers.Assets.at(path, file)

  def checkApp = Action.async {
    Account.col.find(Json.obj()).one[Account].map {
      case Some(wummel) => resOK()
      case None         => resKO("database connection down!")
    }
  }
}