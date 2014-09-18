package controllers

import helper.MongoCollections
import helper.ResultHelper._
import models.{ Account, FileMeta, Identity, TestUserNotification }
import play.api.Play.current
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.{ Logger, Play }
import traits.ExtendedController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

/**
 * User: Björn Reimer
 * Date: 24.06.14
 * Time: 16:35
 */
object TestUserController extends ExtendedController {

  val testUserPrefix = Play.configuration.getString("testUser.prefix").getOrElse("foo").toLowerCase

  def deleteAccount(account: Account): Future[Boolean] = {

    Logger.debug("deleting TestUserId: " + account.loginName)

    Identity.findAll(Json.obj("accountId" -> account.id)).flatMap { list =>
      val deletedIdentities = list.map {
        identity =>
          // delete Avatar
          identity.avatar match {
            case None         => // do nothing
            case Some(fileId) => FileMeta.deleteWithChunks(fileId)
          }

          // delete external contacts
          identity.contacts.map {
            contact =>
              Identity.find(contact.identityId).map {
                case Some(contactIdentity) if contactIdentity.accountId.isEmpty =>
                  // delete avatar
                  contactIdentity.avatar match {
                    case None           => // do nothing
                    case Some(avatarId) => FileMeta.deleteWithChunks(avatarId)
                  }
                  Identity.delete(contactIdentity.id)
                case _ => // do nothing
              }
          }

          // delete all conversations which involve this identity
          val conversationQuery = Json.obj("recipients.identityId" -> identity.id)
          MongoCollections.conversationCollection.remove(conversationQuery)

          // delete all notifications
          val notificationQuery = Json.obj("identityId" -> identity.id)
          MongoCollections.testUserNotificationCollection.remove(notificationQuery)

          // delete identity
          Identity.delete(identity.id).map(_.ok)
      }

      // delete account
      Future.sequence(deletedIdentities).flatMap {
        le =>
          Account.delete(account.id).map(_.ok)
      }
    }
  }

  def deleteTestUser(id: String) = Action.async {
    request =>
      val accountQuery = Json.obj("loginName" -> (testUserPrefix + "_" + id.toLowerCase))
      Account.find(accountQuery).map {
        case None => resNotFound("Test user")
        case Some(account) =>
          deleteAccount(account)
          resOk("deleting")
      }
  }

  def deleteAllTestUsers() = Action.async {
    request =>
      val testUserPrefix = Play.configuration.getString("testUser.prefix").getOrElse("foo").toLowerCase
      val query = Json.obj("loginName" -> Json.obj("$regex" -> ("^" + testUserPrefix), "$options" -> "i"))
      Account.findAll(query).map {
        list =>
          list.seq.foreach {
            account =>
              Await.result(deleteAccount(account), 10.minutes)
          }
          resOk("deleting")
      }
  }

  def getNotifications(id: String) = Action.async {
    request =>
      val accountQuery = Json.obj("loginName" -> (testUserPrefix + "_" + id))

      MongoCollections.accountCollection.find(accountQuery).one[Account].flatMap {
        case None => Future(resNotFound("Test user"))
        case Some(account) =>
          Identity.findAll(Json.obj("accountId" -> account.id)).flatMap {
            identities =>

              val query = Json.obj("$or" -> identities.map(i => Json.obj("identityId" -> i.id)))
              TestUserNotification.findAll(query).map { seq =>
                // delete them
                TestUserNotification.deleteAll(query)

                resOk(seq.map(_.toJson))
              }
          }
      }
  }
}
