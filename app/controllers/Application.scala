package controllers

import helper.DbAdminUtilities
import helper.ResultHelper._
import models.Account
import play.api.Play
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.Future

object Application extends Controller {

  def redirect(url: String) = Action {
    Redirect(url)
  }

  def index = Action.async {
    request =>
      Future(Ok(views.html.index(DbAdminUtilities.mongoVersion)))
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

  def checkApp = Action.async {
    Account.col.find(Json.obj()).one[Account].map {
      case Some(wummel) => resOk()
      case None         => resServerError("database connection down!")
    }
  }
}