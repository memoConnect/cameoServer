package controllers

import traits.ExtendedController

import play.api.libs.json._
import models.{Identity, Contact}
import helper.{OutputLimits, AuthAction}
import scala.concurrent.{ExecutionContext, Future}
import helper.ResultHelper._
import scala.Some
import ExecutionContext.Implicits.global


/**
 * User: Björn Reimer
 * Date: 6/14/13
 * Time: 5:06 PM
 */
object ContactController extends ExtendedController {

  def addContact() = AuthAction.async(parse.tolerantJson) {
    request =>
      val jsBody: JsValue = request.body

      (jsBody \ "identity").validate[Identity].map {
        identity =>
          jsBody.validate[Contact](Contact.createReads(identity.id)).map {
            contact => {
              request.identity.addContact(contact)
              contact.toJson.map(js => resOK(js))
            }
          }.recoverTotal(e => Future.successful(BadRequest(resKO(JsError.toFlatJson(e)))))
      }.recoverTotal(e => Future.successful(BadRequest(resKO(JsError.toFlatJson(e)))))
  }

  def getContact(contactId: String) = AuthAction.async {
    request =>

      val res = request.identity.contacts.find(
        contact => contact.id.toString.equals(contactId)
      )
      res match {
        case None => Future(NotFound(resKO("contact not found")))
        case Some(contact) => contact.toJsonResult
      }
  }

  def getContacts(offset: Int, limit: Int) = AuthAction.async {
    request =>
      val contacts = OutputLimits.applyLimits(request.identity.contacts, offset, limit)

     Future.sequence(contacts.map(_.toJson)).map {
       c => resOK(c)
     }

  }

  def getGroup(group: String, offset: Int, limit: Int) = AuthAction.async {
    request =>
      val filtered = request.identity.contacts.filter(_.groups.contains(group))
      val contacts = OutputLimits.applyLimits(filtered, offset, limit)

      Future.sequence(contacts.map(_.toJson)).map {
        c => resOK(c)
      }
  }

  def getGroups = AuthAction {
    request =>
      val groups = request.identity.contacts.flatMap(_.groups).distinct
      resOK(Json.toJson(groups))
  }


}