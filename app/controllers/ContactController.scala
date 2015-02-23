package controllers

import java.util.Date

import constants.Contacts._
import events._
import helper.JsonHelper._
import helper.ResultHelper._
import helper.{ CheckHelper, IdHelper, OutputLimits }
import models._
import play.api.Logger
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.Result
import services.AuthenticationActions.AuthAction
import services.AvatarGenerator
import traits.ExtendedController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 6/14/13
 * Time: 5:06 PM
 */
object ContactController extends ExtendedController {

  case class CreateExternalContact(displayName: Option[String],
                                   phoneNumber: Option[String],
                                   email: Option[String],
                                   mixed: Option[String])

  object CreateExternalContact {
    implicit def reads(implicit stringReads: Reads[String]): Reads[CreateExternalContact] = (
      (__ \ 'displayName).readNullable[String] and
      (__ \ 'phoneNumber).readNullable[String](verifyPhoneNumber andThen stringReads) and
      (__ \ 'email).readNullable[String](verifyMail andThen stringReads) and
      (__ \ 'mixed).readNullable[String]
    )(CreateExternalContact.apply _)
  }

  def addContact() = AuthAction(includeContacts = true).async(parse.tolerantJson) {
    request =>

      def addExternalContact(js: JsObject): Future[Result] = {
        validateFuture(js, CreateExternalContact.reads) {
          create =>
            def createIdentityAndAddContact(displayName: Option[String], phoneNumber: Option[String], email: Option[String]): Future[Result] = {
              val identity = Identity.create(None, IdHelper.generateCameoId, email, phoneNumber, isDefaultIdentity = true, displayName)
              Identity.col.insert(identity).flatMap { error =>
                error.ok match {
                  case false => Future(resServerError("could not save new identity"))
                  case true =>
                    AvatarGenerator.generate(identity)
                    createContact(identity)
                }
              }
            }

            // use mixed field if there is no phone number and email
            (create.email, create.phoneNumber, create.mixed) match {
              case (None, None, Some(mixed)) =>
                CheckHelper.checkAndCleanMixed(mixed) match {
                  case Some(Left(tel))    => createIdentityAndAddContact(create.displayName, Some(tel), None)
                  case Some(Right(email)) => createIdentityAndAddContact(create.displayName, None, Some(email))
                  case None               => Future(resBadRequest("Neither phonenumber nor email: " + mixed))
                }
              case _ => createIdentityAndAddContact(create.displayName, create.phoneNumber, create.email)
            }
        }
      }

      def addInternalContact(identityId: String): Future[Result] = {
        // check if the user already has this contact
        request.identity.contacts.exists(_.identityId.id.equals(identityId)) match {
          case true => Future(resKo("identity is already in address book"))
          case false =>
            // check if identity exists
            Identity.find(new MongoId(identityId)).flatMap {
              case None => Future(resNotFound("identity"))
              case Some(i) =>
                createContact(i)
            }
        }
      }

      def createContact(identity: Identity): Future[Result] = {
        validateFuture(request.body, Contact.createReads(identity.id)) {
          contact =>
            {
              request.identity.addContact(contact).map {
                case false => resBadRequest("could not create contact")
                case true  => resOk(contact.toJsonWithIdentity(Map(), Seq(identity)))
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

  def editContact(contactId: String) = AuthAction(includeContacts = true).async(parse.tolerantJson) {
    request =>
      val maybeContact = request.identity.contacts.find(contact => contact.id.toString.equals(contactId))
      maybeContact match {
        case None => Future(resNotFound("contact"))
        case Some(contact) =>
          validateFuture(request.body, ContactModelUpdate.format) {
            contactUpdate =>
              contact.update(contactUpdate).map {
                case true  => resOk("updated")
                case false => resBadRequest("cannot update")
              }
          }
      }
  }

  def getContact(contactId: String) = AuthAction(includeContacts = true).async {
    request =>
      val res = request.identity.contacts.find(contact => contact.id.toString.equals(contactId))
      res match {
        case None => Future(resNotFound("contact"))
        case Some(contact) =>
          // get identity behind contact
          Identity.find(contact.identityId).map {
            case None => resServerError("could not find identity")
            case Some(identity) =>
              val res = contact.toJsonWithIdentity(request.identity.publicKeySignatures, Seq(identity))
              resOk(res)
          }
      }
  }

  def getContacts(offset: Int, limit: Int) = AuthAction(includeContacts = true).async {
    request =>

      // get all pending friendRequests, todo: this can be done more efficiently
      val query = Json.obj("friendRequests.identityId" -> request.identity.id)
      val futurePendingFriendRequests = Identity.findAll(query)

      for {
        futurePendingContacts <- futurePendingFriendRequests.map {
          _.map {
            identity =>
              Contact.create(identity.id, id = Some(new MongoId(""))).toJson ++
                Json.obj("identity" -> identity.toPublicJson(request.identity.publicKeySignatures)) ++
                Json.obj("contactType" -> CONTACT_TYPE_PENDING)
          }
        }
        identities <- request.identity.getContactIdentities
      } yield {
        // use identities to get contact jsons
        val contactJsons = request.identity.contacts.map {
          _.toJsonWithIdentity(request.identity.publicKeySignatures, identities)
        }

        val all = contactJsons ++ futurePendingContacts
        // remove all empty elements (they result from deleted identities)
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

  def deleteContact(contactId: String) = AuthAction(includeContacts = true).async {
    request =>
      val res = request.identity.contacts.find(contact => contact.id.toString.equals(contactId))

      res match {
        case None => Future(resNotFound("contact"))
        case Some(c) =>
          // check if that contact is internal or external
          Identity.find(c.identityId).flatMap {
            case None => Future(resNotFound("identity"))
            case Some(contactIdentity) =>

              // delete identity details if it is an external contact
              if (contactIdentity.accountId.isEmpty) {
                contactIdentity.deleteDetails(deleteDisplayName = false)
              }

              request.identity.deleteContact(c.id).map {
                case false => resServerError("unable to delete")
                case true =>
                  // send event
                  actors.eventRouter ! ContactDeleted(request.identity.id, c.id)
                  resOk("deleted")
              }
          }
      }
  }

  //  def getGroup(group: String, offset: Int, limit: Int) = AuthAction(includeContacts = true).async {
  //    request =>
  //      val group = request.identity.getGroup(group)
  //      val limited = OutputLimits.applyLimits(group, offset, limit)
  //
  //      Future.sequence(limited.map(_.toJsonWithIdentity(Some(request.identity.publicKeySignatures)))).map {
  //        c => resOk(c)
  //      }
  //  }
  //
  //  def getGroups = AuthAction(includeContacts = true).async {
  //    request =>
  //      val groups = request.identity.getGroups
  //      Future(resOk(Json.toJson(groups)))
  //  }

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
  def sendFriendRequest = AuthAction(includeContacts = true).async(parse.tolerantJson) {
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
                  case true => Future(resOk(""))
                  case false =>
                    // check if there already is a friend request of this identity
                    other.friendRequests.exists(_.identityId.equals(request.identity.id)) match {
                      case true => Future(resKo("friendRequest already exists"))
                      case false =>
                        val fr = new FriendRequest(request.identity.id, message, new Date)
                        other.addFriendRequest(fr).map {
                          case true =>
                            actors.eventRouter ! FriendRequestNew(receiver, fr, request.identity, receiver)
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

  def answerFriendRequest = AuthAction(includeContacts = true).async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, AnswerFriendRequest.format) {
        afr =>
          request.identity.friendRequests.find(_.identityId.toString.equals(afr.identityId)) match {
            case None => Future(resBadRequest("no friendRequests from this identityId"))
            case Some(o) => afr.answerType match {
              case FRIEND_REQUEST_REJECT =>
                // delete friend request and send events
                request.identity.deleteFriendRequest(new MongoId(afr.identityId))
                actors.eventRouter ! FriendRequestRejected(request.identity.id, MongoId(afr.identityId), request.identity.id)
                actors.eventRouter ! FriendRequestRejected(MongoId(afr.identityId), MongoId(afr.identityId), request.identity.id)
                Future(resOk("rejected"))
              case FRIEND_REQUEST_ACCEPT =>
                // add contact to both identities
                request.identity.deleteFriendRequest(new MongoId(afr.identityId))
                Identity.findWith(MongoId(afr.identityId), includeContacts = true).flatMap {
                  case None => Future(resNotFound("other identity"))
                  case Some(otherIdentity) =>
                    val contact = Contact.create(otherIdentity.id)
                    val otherContact = Contact.create(request.identity.id)
                    // check if accepting identity also has send a friendRequest and remove it
                    otherIdentity.deleteFriendRequest(request.identity.id)
                    for {
                      le1 <- otherIdentity.addContact(otherContact)
                      le2 <- request.identity.addContact(contact)
                    } yield {
                      le1 && le2 match {
                        case true =>
                          val contactJs = contact.toJsonWithIdentity(Map(), Seq(otherIdentity, request.identity))
                          val otherContactJs = otherContact.toJsonWithIdentity(Map(), Seq(otherIdentity, request.identity))
                          // send event to both parties
                          actors.eventRouter ! FriendRequestAccepted(otherIdentity.id, otherIdentity.id, request.identity.id, otherContactJs)
                          actors.eventRouter ! FriendRequestAccepted(request.identity.id, otherIdentity.id, request.identity.id, contactJs)
                          resOk("added contacts")
                        case false => resKo("duplicate entries")
                      }
                    }
                }
              case FRIEND_REQUEST_IGNORE =>
                // delete friend request and add identity to ignore list
                request.identity.deleteFriendRequest(new MongoId(afr.identityId))
                request.identity.addIgnored(new MongoId(afr.identityId))
                Future(resOk(""))
              case _ => Future(resBadRequest("invalid answer type"))
            }
          }
      }
  }

  def deleteFriendRequest(id: String) = AuthAction().async {
    request =>

      // find other identity
      Identity.find(id).map {
        case None => resNotFound("identity")
        case Some(otherIdentity) =>
          // check if that other identity actually has a friend request from this identity
          otherIdentity.friendRequests.find(_.identityId.equals(request.identity.id)) match {
            case None => resBadRequest("Friend request does not exist")
            case Some(friendRequest) =>
              // delete friend request
              otherIdentity.deleteFriendRequest(request.identity.id)
              // send events to both identities
              actors.eventRouter ! FriendRequestDeleted(request.identity.id, request.identity.id, otherIdentity.id)
              actors.eventRouter ! FriendRequestDeleted(otherIdentity.id, request.identity.id, otherIdentity.id)

              resOk("deleted")
          }
      }
  }
}
