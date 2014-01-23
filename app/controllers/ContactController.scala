package controllers

import traits.{OutputLimits, ExtendedController}

import play.api.libs.json._
import play.api.libs.json.Reads._
import models.{Token, Contact}
import helper.{AuthRequest, AuthAction}
import scala.concurrent.Future
import play.api.mvc.SimpleResult
import play.api.libs.concurrent.Execution.Implicits._

/**
 * User: Björn Reimer
 * Date: 6/14/13
 * Time: 5:06 PM
 */
object ContactController extends ExtendedController {

  def checkIfAllowed[A](action: => Future[SimpleResult])(implicit request: AuthRequest[A]): Future[SimpleResult] = {

    // we can only do this if there is a user account associated with this token
    if (request.token.username.isDefined) {
      action
    } else {
      Future.successful(Unauthorized(resKO("No user account")))
    }
  }

  def addContact() = AuthAction.async(parse.tolerantJson) {
    implicit request => checkIfAllowed {
        val jsBody: JsValue = request.body
        jsBody.validate[Contact](Contact.inputReads).map {
          contact =>
            val query = Json.obj("username" -> request.token.username.get)
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
        }.recoverTotal(e => Future(BadRequest(JsError.toFlatJson(e))))

    }
  }

  def getContact(contactId: String, token: String) = AuthAction.async {
    implicit request => checkIfAllowed {
        val query = Json.obj("username" -> request.token.username.get) ++ Json.obj("contacts.contactId" -> contactId)
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

  def getContacts(token: String, offset: Int, limit: Int) = AuthAction.async(parse.tolerantJson) {
    implicit request => checkIfAllowed {
        implicit val outputLimits = OutputLimits(offset, limit)
        Contact.getArray("username", request.token.username.get, "contacts").map {
          case None => BadRequest(resKO("Unable to get contacts"))
          case Some(contacts) => Ok(resOK(Contact.toSortedJsonArray(contacts)))
        }
    }
  }

  def getGroup(group: String, token: String, offset: Int, limit: Int) = AuthAction.async(parse.tolerantJson) {
    implicit request => checkIfAllowed {
        implicit val outputLimits = OutputLimits(offset, limit)
        Contact.getArray("username", request.token.username.get, "contacts").map {
          case None => BadRequest(resKO("Unable to get contacts"))
          case Some(contacts) => {
            val filtered = contacts.filter(_.groups.contains(group))
            Ok(resOK(Contact.toSortedJsonArray(filtered)))
          }
        }
      }
  }

  def getGroups(token: String) = AuthAction.async(parse.tolerantJson) {
    implicit request => checkIfAllowed {
        Contact.getArray("username", request.token.username.get, "contacts").map {
          case None => BadRequest(resKO("Unable to get contacts"))
          case Some(contacts) => {
            val groups = contacts.flatMap(_.groups).distinct
            Ok(resOK(JsArray(groups.sorted.map(JsString))))
          }
        }
      }
  }
}