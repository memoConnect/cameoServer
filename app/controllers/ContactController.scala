package controllers

import traits.ExtendedController

import play.api.libs.json._
import play.api.libs.json.Reads._
import models.Contact
import play.api.Logger

/**
 * User: Björn Reimer
 * Date: 6/14/13
 * Time: 5:06 PM
 */
object ContactController extends ExtendedController {

  def addContact = authenticatePOST() {
    (username, request) =>
      val jsBody: JsValue = request.body
      jsBody.validate[Contact](Contact.inputReads).map {
        contact =>
          Async {
            val query = Json.obj("username" -> username)
            val set = Json.obj("$push" -> Json.obj("contacts" -> Json.toJson(contact)))
            userCollection.update(query, set).map {
              lastError => {
                if (lastError.ok) {
                  Ok(resOK(Json.toJson(contact)(Contact.outputWrites)))
                } else {
                  InternalServerError(resKO("MongoError: " + lastError))
                }
              }
            }
          }
      }.recoverTotal(e => BadRequest(JsError.toFlatJson(e)))
  }

  def getContact(contactId: String, token: String) = authenticateGET(token) {
    (username, request) => Async {
      val query = Json.obj("username" -> username) ++ Json.obj("contacts.contactId" -> contactId)
      val filter = Json.obj("contacts.$" -> 1)

      userCollection.find(query, filter).one[JsObject].map {
        case None => NotFound(resKO("The contact does not exist"))
        case Some(user: JsObject) => (user \ "contacts")(0).asOpt[Contact] match {
          case None => NotFound(resKO("The contact does not exist"))
          case Some(contact: Contact) => Ok(resOK(Contact.toJson(contact)))
        }
      }
    }
  }

  def getContacts(token: String) = authenticateGET(token) {
    (username, request) =>
      Async {
        val futureContacts = Contact.getArray("username", username, "contacts")
        futureContacts.map {
          contactsOpt => contactsOpt match {
            case None => BadRequest(resKO("Unable to get contacts"))
            case Some(contacts) => Ok(resOK(Contact.toSortedJsonArray(contacts)))
          }
        }
      }
  }

  def getGroup(group: String, token: String) = authenticateGET(token) {
    (username, request) =>
      Async {
        val futureContacts = Contact.getArray("username", username, "contacts")
        futureContacts.map {
          contactsOpt => contactsOpt match {
            case None => BadRequest(resKO("Unable to get contacts"))
            case Some(contacts) => {
              val filtered = contacts.filter(_.groups.contains(group))
              Ok(resOK(Contact.toSortedJsonArray(filtered)))
            }
          }
        }
      }
  }

  def getGroups(token: String) = authenticateGET(token) {
    (username, request) =>
      Async {
        val futureContacts = Contact.getArray("username", username, "contacts")
        futureContacts.map {
          contactsOpt => contactsOpt match {
            case None => BadRequest(resKO("Unable to get contacts"))
            case Some(contacts) => {
              val groups = contacts.flatMap(_.groups).distinct
              Ok(resOK(JsArray(groups.sorted.map(JsString(_)))))
            }
          }
        }
      }
  }
}