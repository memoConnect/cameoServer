package controllers

import helper.CmActions.AuthAction
import helper.OutputLimits
import helper.ResultHelper._
import models._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.mvc.Action
import traits.ExtendedController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 1/20/14
 * Time: 12:07 PM
 */
object IdentityController extends ExtendedController {

  def getIdentity(id: String) = Action.async {
    //todo: external identities should only be available to contact owner and other participants in the conversation
    val mongoId = new MongoId(id)

    Identity.find(mongoId).map {
      case None           => resNotFound("identity")
      case Some(identity) => resOk(identity.toPublicJson)
    }
  }

  def getOwnIdentity = AuthAction(allowExternal = true) {
    request => resOk(request.identity.toPrivateJson)
  }

  def updateIdentity() = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture[IdentityUpdate](request.body, IdentityUpdate.reads) {
        identityUpdate =>
          {
            request.identity.update(identityUpdate).map {
              case false => resServerError("nothing updated")
              case true  => resOk("updated")
            }
          }
      }
  }

  def search(offset: Int, limit: Int) = AuthAction().async(parse.tolerantJson) {
    request =>

      case class IdentitySearch(search: String, fields: Seq[String], excludeContacts: Option[Boolean])

      def reads: Reads[IdentitySearch] = (
        (__ \ 'search).read[String](minLength[String](4)) and
        (__ \ 'fields).read[Seq[String]] and
        (__ \ 'excludeContacts).readNullable[Boolean]
      )(IdentitySearch.apply _)

      validateFuture(request.body, reads) {
        identitySearch =>
          // there needs to be at least one field
          identitySearch.fields.isEmpty match {
            case true => Future(resBadRequest("at least one element in fields required"))
            case false =>
              val cameoId = if (identitySearch.fields.contains("cameoId")) Some(identitySearch.search) else None
              val displayName = if (identitySearch.fields.contains("displayName")) Some(identitySearch.search) else None

              // find all pending friend request
              Identity.findAll(Json.obj("friendRequests.identityId" -> request.identity.id)).flatMap {
                pendingFriendRequest =>
                  val exclude =
                    Seq(request.identity.id) ++
                      pendingFriendRequest.map(_.id) ++
                      // exclude identities that requested friendship
                      request.identity.friendRequests.map(_.identityId) ++
                      // exclude contacts
                      {
                        identitySearch.excludeContacts match {
                          case Some(true) => request.identity.contacts.map(_.identityId)
                          case _          => Seq()
                        }
                      }

                  Identity.search(cameoId, displayName).map {
                    list =>
                      // filter excluded identities
                      val filtered = list.filterNot(identity => exclude.exists(_.equals(identity.id)))
                      val limited = OutputLimits.applyLimits(filtered, offset, limit)
                      resOk(limited.map { i => i.toPublicJson })
                  }
              }
          }
      }
  }
}
