package controllers

import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.Json
import helper.{ MongoCollections, DbAdminUtilities }
import play.api.{Logger, Play}
import helper.ResultHelper._
import models.{ Identity, Account }
import play.api.Play.current
import reactivemongo.bson.BSONRegex

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

  def deleteTestUser(id: String) = Action.async {
    request =>
      val prefix = Play.configuration.getString("testUser.prefix").getOrElse("foo").toLowerCase

      val accountQuery = Json.obj("loginName" -> (prefix + "_" + id))
      MongoCollections.accountCollection.find(accountQuery).one[Account].map {
        case None => resNotFound("Test user")
        case Some(account) =>
          // delete all identities
          val identityQuery = Json.obj("$or" -> account.identities.map(i => Json.obj("_id" -> i)))
          MongoCollections.identityCollection.remove(identityQuery)

          // delete all conversations which involve this test user
          val conversationQuery = Json.obj("$or" -> account.identities.map(i => Json.obj("recipients.identityId" -> i)))
          MongoCollections.conversationCollection.remove(conversationQuery)

          // delete account
          MongoCollections.accountCollection.remove(Json.obj("_id" -> account.id))

          resOk("deleted")
      }
  }

  def checkApp = Action.async {
    Account.col.find(Json.obj()).one[Account].map {
      case Some(wummel) => resOK()
      case None         => resKo("database connection down!")
    }
  }
}