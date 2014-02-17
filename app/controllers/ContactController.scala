package controllers

import traits.ExtendedController

import play.api.libs.json._
import models.{ContactUpdate, MongoId, Identity, Contact}
import helper.{ OutputLimits, AuthAction }
import scala.concurrent.{ ExecutionContext, Future }
import helper.ResultHelper._
import scala.Some
import ExecutionContext.Implicits.global
import constants.Contacts._
import play.api.Logger

/**
 * User: Björn Reimer
 * Date: 6/14/13
 * Time: 5:06 PM
 */
object ContactController extends ExtendedController {

  def addContact() = AuthAction.async(parse.tolerantJson) {
    request =>
      val jsBody: JsValue = request.body

      // check if an identity id is given
      val (contactType, maybeIdentity): (String, Future[Option[(Identity)]]) = (jsBody \ "identityId").asOpt[String] match {
        case Some(id) => {
          // check if identity exists
          (CONTACT_TYPE_INTERNAL,Identity.find(id))
        }
        case None => {
          // if not check if there is a valid identity object
          val i = (jsBody \ "identity").validate[Identity](Identity.createReads).map {
            identity =>
              {
                Identity.col.insert(identity).map {
                  lastError => Some(identity)
                }
              }
          }.recoverTotal(e => Future(None))
          (CONTACT_TYPE_EXTERNAL, i)
        }

      }

      // read contact and add identity id
      maybeIdentity.flatMap {
        case None => Future(resBadRequest("invalid identity"))
        case Some(identity) => {
          validateFuture(jsBody, Contact.createReads(identity.id, contactType)) {
            contact =>
              {
                request.identity.addContact(contact)
                contact.toJsonWithIdentity.map(js => resOK(js))
              }
          }
        }
      }
  }

  def editContact(contactId: String) = AuthAction(parse.tolerantJson) {
    request =>
      val res = request.identity.contacts.find(contact => contact.id.toString.equals(contactId))

      res match {
        case None          => resNotFound("contact not found")
        case Some(contact) => {
          // check if we are allowed to edit it
          contact.contactType match {
            case CONTACT_TYPE_INTERNAL => resUnauthorized("cannot edit contact details of cameo user")
            case CONTACT_TYPE_EXTERNAL => validate(request.body, ContactUpdate.format) {
              updateContact =>
                contact.update(updateContact)
                resOK()
            }
          }
        }
      }
  }

  def getContact(contactId: String) = AuthAction.async {
    request =>

      val res = request.identity.contacts.find(contact => contact.id.toString.equals(contactId))

      res match {
        case None          => Future(resNotFound("contact not found"))
        case Some(contact) => contact.toJsonWithIdentityResult
      }
  }

  def getContacts(offset: Int, limit: Int) = AuthAction.async {
    request =>
      val contacts = OutputLimits.applyLimits(request.identity.contacts, offset, limit)

      Future.sequence(contacts.map(_.toJsonWithIdentity)).map {
        c => resOK(c)
      }
  }

  def getGroup(group: String, offset: Int, limit: Int) = AuthAction.async {
    request =>

      val contacts = request.identity.getGroup(group)
      val limited = OutputLimits.applyLimits(contacts, offset, limit)

      Future.sequence(limited.map(_.toJsonWithIdentity)).map {
        c => resOK(c)
      }
  }

  def getGroups = AuthAction {
    request =>
      val groups = request.identity.getGroups
      resOK(Json.toJson(groups))
  }

}