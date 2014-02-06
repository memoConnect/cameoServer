package controllers

import traits.ExtendedController

import play.api.libs.json._
import models.{ MongoId, Identity, Contact }
import helper.{ OutputLimits, AuthAction }
import scala.concurrent.{ ExecutionContext, Future }
import helper.ResultHelper._
import scala.Some
import ExecutionContext.Implicits.global
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
      val identityId: Option[MongoId] = (jsBody \ "identityId").asOpt[String] match {
        case Some(id) => Some(new MongoId(id))
        case None => {
          // if not check if there is a valid identity object
          (jsBody \ "identity").validate[Identity](Identity.createReads).map {
            identity =>
              {
                Identity.col.insert(identity)
                Some(identity.id)
              }
          }.recoverTotal(e => None)
        }
      }

      // read contact and add identity id
      identityId match {
        case None => Future(BadRequest(resKO("invalid identity")))
        case Some(id) => {
          jsBody.validate[Contact](Contact.createReads(id)).map {
            contact =>
              {
                request.identity.addContact(contact)
                contact.toJsonWithIdentity.map(js => resOK(js))
              }
          }.recoverTotal(e => Future.successful(BadRequest(resKO(JsError.toFlatJson(e)))))
        }
      }
  }

  def getContact(contactId: String) = AuthAction.async {
    request =>

      val res = request.identity.contacts.find(
        contact => contact.id.toString.equals(contactId))
      res match {
        case None          => Future(NotFound(resKO("contact not found")))
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
      resOK(Json.toJson(groups))
  }

}