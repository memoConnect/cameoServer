package controllers

import helper.MongoCollections
import helper.ResultHelper._
import models.{ Account, TestUserNotification }
import play.api.Play.current
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.{ Logger, Play }
import traits.ExtendedController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 24.06.14
 * Time: 16:35
 */
object TestUserController extends ExtendedController {

  val testUserPrefix = Play.configuration.getString("testUser.prefix").getOrElse("foo").toLowerCase

  def deleteTestUser(id: String) = Action.async {
    request =>

      Logger.debug("deleting TestUserId: " + id)

      val accountQuery = Json.obj("loginName" -> (testUserPrefix + "_" + id.toLowerCase))
      MongoCollections.accountCollection.find(accountQuery).one[Account].map {
        case None => resNotFound("Test user")
        case Some(account) =>
          // delete all identities
          val identityQuery = Json.obj("$or" -> account.identities.map(i => Json.obj("_id" -> i)))
          MongoCollections.identityCollection.remove(identityQuery)

          // delete all conversations which involve this test user
          val conversationQuery = Json.obj("$or" -> account.identities.map(i => Json.obj("recipients.identityId" -> i)))
          MongoCollections.conversationCollection.remove(conversationQuery)

          // delete all notifications
          val notificationQuery = Json.obj("$or" -> account.identities.map(i => Json.obj("identityId" -> i)))
          MongoCollections.testUserNotificationCollection.remove(notificationQuery)

          // delete account
          MongoCollections.accountCollection.remove(Json.obj("_id" -> account.id))

          resOk("deleted")
      }
  }

  def getNotifications(id: String) = Action.async {
    request =>
      val accountQuery = Json.obj("loginName" -> (testUserPrefix + "_" + id))

      MongoCollections.accountCollection.find(accountQuery).one[Account].flatMap {
        case None => Future(resNotFound("Test user"))
        case Some(account) =>
          val query = Json.obj("$or" -> account.identities.map(i => Json.obj("identityId" -> i)))
          TestUserNotification.findAll(query).map { seq =>
            resOk(seq.map(_.toJson))
          }
      }
  }
}
