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
          // get all identities of this account
          val futureIdentities = account.identities.map { id =>
            Identity.find(id)
          }

          Future.sequence(futureIdentities).map {
            _.filter(_.isDefined).map(_.get).map {
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
                Identity.delete(identity.id)
            }
          }
          // delete account
          Account.delete(account.id)

          resOk("deleting")
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
            // delete them
            TestUserNotification.deleteAll(query)

            resOk(seq.map(_.toJson))
          }
      }
  }
}
