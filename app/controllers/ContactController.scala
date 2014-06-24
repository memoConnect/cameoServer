package controllers

import traits.ExtendedController

import play.api.libs.json._
import models._
import helper.OutputLimits
import helper.CmActions.AuthAction
import scala.concurrent.{ ExecutionContext, Future }
import helper.ResultHelper._
import ExecutionContext.Implicits.global
import constants.Contacts._
import scala.Some
import play.api.mvc.Result
import play.api.libs.json.JsObject
import java.util.Date
import play.api.Logger
import actors.NewFriendRequest
import services.AvatarGenerator

/**
 * User: Björn Reimer
 * Date: 6/14/13
 * Time: 5:06 PM
 */
object ContactController extends ExtendedController {

  def addContact() = AuthAction().async(parse.tolerantJson) {
    request =>

      def addExternalContact(js: JsObject): Future[Result] = {
        validateFuture(js, Identity.createReads) { identity =>
          Identity.col.insert(identity).flatMap { error =>
            error.ok match {
              case false => Future(resServerError("could not save new identity"))
              case true =>
                AvatarGenerator.generate(identity)
                createContact(identity.id, CONTACT_TYPE_EXTERNAL)
            }
          }
        }
      }

      def addInternalContact(identityId: String): Future[Result] = {
        // check if the user already has this contact
        request.identity.contacts.exists(_.identityId.toString.equals(identityId)) match {
          case true => Future(resKo("identity is already in address book"))
          case false =>
            // check if identity exists
            Identity.find(new MongoId(identityId)).flatMap {
              case None => Future(resNotFound("identity"))
              case Some(i) =>
                createContact(i.id, CONTACT_TYPE_INTERNAL)
            }
        }
      }

      def createContact(identityId: MongoId, contactType: String): Future[Result] = {
        validateFuture(request.body, Contact.createReads(identityId, contactType)) {
          contact =>
            {
              request.identity.addContact(contact).flatMap {
                case false => Future(resBadRequest("could not create contact"))
                case true  => contact.toJsonWithIdentity.map(js => resOk(js))
              }
            }
        }
      }

      // check if the contact is internal or external
      (request.body \ "identityId").asOpt[String] match {
        case Some(id) => addInternalContact(id)
        case None =>
          (request.body \ "identity").asOpt[JsObject] match {
            case None     => Future(resBadRequest("no identityId or identity object"))
            case Some(js) => addExternalContact(js)
          }
      }

  }

  def editContact(contactId: String) = AuthAction().async(parse.tolerantJson) {
    request =>
      val maybeContact = request.identity.contacts.find(contact => contact.id.toString.equals(contactId))
      maybeContact match {
        case None => Future(resNotFound("contact"))
        case Some(contact) =>
          validateFuture(request.body, ContactUpdate.format) {
            contactUpdate =>
              contact.update(contactUpdate).map {
                case true  => resOk("updated")
                case false => resBadRequest("cannot update")
              }
          }
      }
  }

  def getContact(contactId: String) = AuthAction().async {
    request =>
      val res = request.identity.contacts.find(contact => contact.id.toString.equals(contactId))

      res match {
        case None          => Future(resNotFound("contact"))
        case Some(contact) => contact.toJsonWithIdentityResult
      }
  }

  def getContacts(offset: Int, limit: Int) = AuthAction().async {
    request =>

      // get all pending friendRequests, todo: this can be done more efficiently
      val query = Json.obj("friendRequests.identityId" -> request.identity.id)
      val futurePendingFriendRequests = Identity.col.find(query).cursor[Identity].collect[Seq]()

      for {
        futurePendingContacts <- futurePendingFriendRequests.map {
          _.map {
            identity =>
              Contact.create(identity.id, id = Some(new MongoId(""))).toJson ++
                Json.obj("identity" -> identity.toPublicJson) ++
                Json.obj("contactType" -> CONTACT_TYPE_PENDING)
          }
        }
        futureContacts <- Future.sequence(request.identity.contacts.map(_.toJsonWithIdentity))
      } yield {
        val all = futureContacts ++ futurePendingContacts
        // remove all empty element (they result from deleted identities)
        val nonEmpty = all.filterNot(_.equals(Json.obj()))
        val sorted = nonEmpty.sortBy(
          js =>
            (js \ "identity" \ "displayName").asOpt[String]
              .getOrElse((js \ "identity" \ "cameoId").asOpt[String]
                .getOrElse({ Logger.error("invalid identity: " + js); "xxx" }))
              .toLowerCase
        )
        val limited = OutputLimits.applyLimits(sorted, offset, limit)
        resOk(limited)
      }
  }

  def deleteContact(contactId: String) = AuthAction().async {
    request =>
      val res = request.identity.contacts.find(contact => contact.id.toString.equals(contactId))

      res match {
        case None => Future(resNotFound("contact"))
        case Some(c) =>
          request.identity.deleteContact(c.id).map {
            case false => resBadRequest("unable to delete")
            case true  => resOk("deleted")
          }
      }
  }

  def getGroup(group: String, offset: Int, limit: Int) = AuthAction().async {
    request =>

      val contacts = request.identity.getGroup(group)
      val limited = OutputLimits.applyLimits(contacts, offset, limit)

      Future.sequence(limited.map(_.toJsonWithIdentity)).map {
        c => resOk(c)
      }
  }

  def getGroups = AuthAction().async {
    request =>
      val groups = request.identity.getGroups
      Future(resOk(Json.toJson(groups)))
  }

  def getFriendRequests = AuthAction().async {
    request =>
      val futureFriendRequests = request.identity.friendRequests.map(_.toJsonWithIdentity)

      Future.sequence(futureFriendRequests).map {
        seq => resOk(seq)
      }
  }

  case class SendFriendRequest(identityId: Option[String],
                               cameoId: Option[String],
                               message: Option[String])

  object SendFriendRequest {
    implicit val reads: Reads[SendFriendRequest] = Json.reads[SendFriendRequest]
  }

  // todo: set maximum size of friend request message
  def sendFriendRequest = AuthAction().async(parse.tolerantJson) {
    request =>
      def executeFriendRequest(receiver: MongoId, message: Option[String]): Future[Result] = {
        // check if the other identity is already in contacts
        request.identity.contacts.exists(_.identityId.equals(receiver)) match {
          case true => Future(resKo("identity is already in address book"))
          case false =>
            // check if identityId exists
            Identity.find(receiver).flatMap {
              case None => Future(resNotFound("identity"))
              case Some(other) =>
                // check if identity is ignored
                other.ignoredIdentities.exists(_.equals(request.identity.id)) match {
                  case true => Future(resOk())
                  case false =>
                    // check if there already is a friend request of this identity
                    other.friendRequests.exists(_.identityId.equals(request.identity.id)) match {
                      case true => Future(resKo("friendRequest already exists"))
                      case false =>
                        val fr = new FriendRequest(request.identity.id, message, new Date)
                        other.addFriendRequest(fr).map {
                          case true =>
                            actors.eventRouter ! NewFriendRequest(receiver, fr)
                            resOk("request added")
                          case false =>
                            resServerError("could not update")
                        }
                    }
                }
            }
        }
      }

      validateFuture(request.body, SendFriendRequest.reads) {
        sfr =>
          (sfr.identityId, sfr.cameoId) match {
            case (None, None)            => Future(resBadRequest("either identityId or cameoId required"))
            case (Some(i), Some(c))      => Future(resBadRequest("only one of identityId and cameoId allowed"))
            case (Some(i: String), None) => executeFriendRequest(new MongoId(i), sfr.message)
            case (None, Some(c: String)) =>
              // search for cameoId and get identityId
              Identity.findByCameoId(c).flatMap {
                case None           => Future(resNotFound("cameoId"))
                case Some(identity) => executeFriendRequest(identity.id, sfr.message)
              }
          }
      }
  }

  case class AnswerFriendRequest(identityId: String, answerType: String)

  object AnswerFriendRequest { implicit val format = Json.format[AnswerFriendRequest] }

  def answerFriendRequest = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, AnswerFriendRequest.format) {
        afr =>
          request.identity.friendRequests.find(_.identityId.toString.equals(afr.identityId)) match {
            case None => Future(resBadRequest("no friendRequests from this identityId"))
            case Some(o) => afr.answerType match {
              case FRIEND_REQUEST_REJECT =>
                // delete friend request and do nothing else
                request.identity.deleteFriendRequest(new MongoId(afr.identityId))
                Future(resOk())
              case FRIEND_REQUEST_ACCEPT =>
                // add contact to both identites
                request.identity.deleteFriendRequest(new MongoId(afr.identityId))
                Identity.find(afr.identityId).flatMap {
                  case None => Future(resNotFound("other identity"))
                  case Some(otherIdentity) =>
                    // check if accepting identity also has send a friendRequest and remove it
                    otherIdentity.deleteFriendRequest(request.identity.id)
                    for {
                      le1 <- otherIdentity.addContact(Contact.create(request.identity.id))
                      le2 <- request.identity.addContact(Contact.create(otherIdentity.id))
                    } yield {
                      le1 && le2 match {
                        case true  => resOk("added contacts")
                        case false => resKo("duplicate entries")
                      }
                    }
                }
              case FRIEND_REQUEST_IGNORE =>
                // delete friend request and add identity to ignore list
                request.identity.deleteFriendRequest(new MongoId(afr.identityId))
                request.identity.addIgnored(new MongoId(afr.identityId))
                Future(resOk())
              case _ => Future(resBadRequest("invalid answer type"))
            }
          }
      }
  }
}
