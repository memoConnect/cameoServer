package controllers

import traits.ExtendedController

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import helper.IdHelper
import scala.concurrent.Future
import play.api.Logger

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

  def outputContacts: Reads[JsObject] =
    (__ \\ 'contacts).json.pickBranch(fromCreated)

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

      val contactId = IdHelper.generateMessageId()

      jsBody.transform(validateContact andThen addCreateDate andThen addContactId(contactId)).map {
        jsContact => {
          val query = Json.obj("username" -> username)
          val set = Json.obj("$set" -> Json.obj("contacts." + contactId -> jsContact))
          Async {
            userCollection.update(query, set).map {
              lastError => {
                if (lastError.ok) {
                  Ok(resOK(Json.obj("contactId" -> contactId)))
                } else {
                  InternalServerError(resKO("MongoError: " + lastError))
                }
              }
            }
          }
        }
      }.recoverTotal {
        error => BadRequest(resKO(JsError.toFlatJson(error)))
      }
  }

  def getContact(contactId: String, token: String) = authenticateGET(token) {
    (username, request) => Async {
      userCollection.find(Json.obj("username" -> username)).one[JsObject].map {
        case None => NotFound(resKO(Json.obj("invalidUser" -> username)))
        case Some(u: JsObject) => (u \ "contacts" \ contactId).asOpt[JsObject] match {
          case None => NotFound(resKO("Contact not found: " + contactId))
          case Some(c: JsObject) =>
            c.transform(outputContact).map {
              jsRes => Ok(resOK(jsRes))
            }.recoverTotal {
              error => InternalServerError(resKO(JsError.toFlatJson(error)))
            }
        }
      }
    }
  }

  def getContacts(token: String) = authenticateGET(token) {
    (username, request) =>
      Async {
        userCollection.find(Json.obj("username" -> username)).one[JsObject].map {
          case None => NotFound(resKO(Json.obj("invalidUser" -> username)))
          case Some(u: JsObject) => {
            u.transform(createArrayFromIdObject("contacts", fromCreated) andThen (__ \ 'contacts).json.pick[JsArray])
              .map {
              jsContacts => Ok(resOK(jsContacts))
            }.recoverTotal {
              error => InternalServerError(resKO(JsError.toFlatJson(error)))
            }
          }
        }
      }
  }

  def getGroup(group: String, token: String) = authenticateGET(token) {
    (username, request) =>
      Async {
        // get contacts of this user
        userCollection.find(Json.obj("username" -> username)).one[JsObject].map {
          case None => NotFound(resKO(Json.obj("invalidUser" -> username)))
          case Some(u: JsObject) => {
            val result: JsArray = JsArray((u \ "contacts").asOpt[JsObject].getOrElse(Json.obj()).fields.foldLeft
              (Seq[JsObject]())((groupContacts: Seq[JsObject], kv) => {
              val groups = (kv._2 \ "groups").asOpt[List[String]].getOrElse(List[String]())
              if (groups.exists(s => s.equals(group))) {
                groupContacts :+ kv._2.transform(__.json.pick(fromCreated)).getOrElse(Json.obj())
              } else {
                groupContacts
              }
            }))
            Ok(resOK(result))
          }
        }
      }
  }

  def getGroups(token: String) = authenticateGET(token) {
    (username, request) =>
      Async {
        // get contacts of this user
        userCollection.find(Json.obj("username" -> username)).one[JsObject].map {
          case None => NotFound(resKO(Json.obj("invalidUser" -> username)))
          case Some(u: JsObject) => {
            val contacts: JsObject = (u \ "contacts").asOpt[JsObject].getOrElse(Json.obj())

            val result = contacts.fields.foldLeft(List[JsString]())((uniqueGroups: List[JsString], kv: (String,
              JsValue)) => {
              val contactGroups = (kv._2 \ "groups").asOpt[List[JsString]].getOrElse(List[JsString]())

              contactGroups.foldLeft(uniqueGroups)((uniqueGroups: List[JsString], group: JsString) => {
                if (uniqueGroups.exists((g: JsString) => g.equals(group))) {
                  uniqueGroups
                } else {
                  uniqueGroups :+ group
                }
              })
            })
            Ok(resOK(JsArray(result)))
          }
        }
      }
  }
}