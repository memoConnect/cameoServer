package controllers

import helper.DbUtilities
import helper.ResultHelper._
import models.Account
import play.api.Play
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.Json
import play.api.mvc._
import services.{ LocalizationMessages, PushdConnector }

import scala.concurrent.Future

object Application extends Controller {

  def redirect(url: String) = Action {
    Redirect(url)
  }

  def index = Action.async {
    request =>
      Future(Ok(views.html.index(DbUtilities.mongoVersion)))
  }

  def dumpDb() = Action {
    Play.isDev match {
      case true =>
        DbUtilities.dumpDb()
        Ok("dumped")
      case false =>
        resBadRequest("not in dev mode")
    }
  }

  def loadFixtures = Action {
    Play.isDev match {
      case true =>
        DbUtilities.loadFixtures()
        Ok("loaded")
      case false =>
        resBadRequest("not in dev mode")
    }
  }

  def migrateAll = Action {
    Play.isDev match {
      case false =>
        resBadRequest("not in dev mode")
      case true =>
        DbUtilities.migrateAll()
        Ok("migrating")
    }
  }

  def checkApp = Action.async {
    for {
      dbConnection <- Account.col.find(Json.obj()).one[Account].map(_.isDefined)
      pushD <- PushdConnector.sendEvent("test", Map(LocalizationMessages.defaultLanguage -> "test"), Map(LocalizationMessages.defaultLanguage -> "test"), "")
    } yield {
      (dbConnection, pushD) match {
        case (false, true)  => resServerError("database down")
        case (true, false)  => resServerError("pushd down")
        case (false, false) => resServerError("database and pushd down")
        case (true, true)   => resOk("all ok")
      }
    }
  }

  def verify = Action.async {
      request =>
        Future(Ok(views.html.verify()))
    }
}