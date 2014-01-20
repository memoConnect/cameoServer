package controllers

import traits.ExtendedController

import play.api.libs.json._
import models.Contact
import helper.{OutputLimits, AuthAction}

/**
 * User: Björn Reimer
 * Date: 6/14/13
 * Time: 5:06 PM
 */
object ContactController extends ExtendedController {

  def addContact() = AuthAction(parse.tolerantJson) {
    request =>
      val jsBody: JsValue = request.body
      jsBody.validate[Contact](Contact.inputReads).map {
        contact =>
          request.identity.addContact(contact)
          Ok(resOK(contact.toJson))
      }.recoverTotal(e => BadRequest(resKO(JsError.toFlatJson(e))))
  }


  def getContact(contactId: String) = AuthAction {
    request =>

      val res = request.identity.contacts.find(
        contact => contact.id.toString.equals(contactId)
      )

      res match {
        case None => NotFound(resKO("contact not found"))
        case Some(contact) => Ok(resOK(contact.toJson))

      }
  }

  def getContacts(offset: Int, limit: Int) = AuthAction {
    request =>
      val contacts = OutputLimits.apply(request.identity.contacts, offset, limit)
      Ok(resOK(contacts.map(_.toJson)))
  }

  def getGroup(group: String, offset: Int, limit: Int) = AuthAction {
    request =>
      val filtered = request.identity.contacts.filter(_.groups.contains(group))
      val out = OutputLimits.apply(filtered, offset, limit)
      Ok(resOK(out.map(_.toJson)))
  }

  def getGroups = AuthAction {
    request =>
      val groups = request.identity.contacts.flatMap(_.groups).distinct
      Ok(resOK(Json.toJson(groups)))
  }


}