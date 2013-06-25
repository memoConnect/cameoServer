package controllers

import traits.ExtendedController

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import scala.concurrent.Future
import models.Contact

/**
 * User: Björn Reimer
 * Date: 6/14/13
 * Time: 5:06 PM
 */
object ContactsController extends ExtendedController {

  /**
   * JSON Transformers
   */
  def addContactId(id: String): Reads[JsObject] = __.json.update((__ \ 'contactId).json.put(JsString(id)))

  val validateContact: Reads[JsObject] =
    (((__ \ 'name).json.pickBranch(Reads.of[JsString]) or emptyObj) and
      ((__ \ 'email).json.pickBranch(Reads.of[JsString]) or emptyObj) and
      ((__ \ 'phonenumber).json.pickBranch(Reads.of[JsString]) or emptyObj) and
      ((__ \ 'groups).json.pickBranch(Reads.of[JsArray]) or emptyObj) and
      ((__ \ 'username).json.pickBranch(Reads.of[JsString]) or emptyObj)).reduce

  def outputContact: Reads[JsObject] = fromCreated

  /**
   * Helper
   */
  def findContact(contactId: String): Future[Option[JsObject]] = {

    val query: JsObject = Json.obj("contacts." + contactId -> Json.obj("$exists" -> true))

    userCollection.find(query, Json.obj("contacts." + contactId -> true)).one[JsObject]
  }

  /**
   * Actions
   */
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
          case Some(contact: Contact) => Ok(resOK(Json.toJson(contact)(Contact.outputWrites)))
        }
      }
    }
  }

  def getContacts(token: String) = authenticateGET(token) {
    (username, request) =>
      Async {
        val futureContacts = getArray[Contact](userCollection, "username", username, "contacts", Contact.defaultReads)
        futureContacts.map {
          contactsOpt => contactsOpt match {
            case None => BadRequest(resKO("Unable to get contacts"))
            case Some(contacts) => Ok(resOK(toSortedArray[Contact](contacts, Contact.outputWrites, _.name)))
          }
        }
      }
  }

  def getGroup(group: String, token: String) = authenticateGET(token) {
    (username, request) =>
      Async {
        val futureContacts = getArray[Contact](userCollection, "username", username, "contacts", Contact.defaultReads)
        futureContacts.map {
          contactsOpt => contactsOpt match {
            case None => BadRequest(resKO("Unable to get contacts"))
            case Some(contacts) => {
              val filtered = contacts.filter(_.groups.contains(group))
              Ok(resOK(toSortedArray[Contact](filtered, Contact.outputWrites, _.name)))
            }

          }
        }
      }
  }

  def getGroups(token: String) = authenticateGET(token) {
    (username, request) =>
      Async {
        val futureContacts = getArray[Contact](userCollection, "username", username, "contacts", Contact.defaultReads)
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