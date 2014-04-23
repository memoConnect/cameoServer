package controllers

import traits.ExtendedController
import models._
import play.api.mvc.Action
import scala.concurrent.{ Future, ExecutionContext }
import ExecutionContext.Implicits.global
import helper.ResultHelper._
import helper.CmActions.AuthAction
import scala.Some

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._

/**
 * User: Björn Reimer
 * Date: 1/20/14
 * Time: 12:07 PM
 */
object IdentityController extends ExtendedController {

  def getIdentityById(id: String) = Action.async {
    val mongoId = new MongoId(id)

    Identity.find(mongoId).map {
      case None           => resNotFound("identity")
      case Some(identity) => resOK(identity.toPublicJson)
    }
  }

  def getIdentityByToken = AuthAction().async {
    request =>

      val mongoId = request.identity.id

      Identity.find(mongoId).map {
        case None           => resNotFound("identity")
        case Some(identity) => resOK(identity.toPrivateJson)
      }
  }

  def updateIdentity() = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture[IdentityUpdate](request.body, IdentityUpdate.reads) {
        identityUpdate =>
          {
            request.identity.update(identityUpdate).map {
              case false => resServerError("nothing updated")
              case true  => resOK("updated")

            }
          }
      }
  }

  def search() = AuthAction().async(parse.tolerantJson) {

    request =>

      case class VerifyRequest(search: String, fields: Seq[String], excludeContacts: Option[Boolean])

      def reads: Reads[VerifyRequest] = (
        (__ \ 'search).read[String](minLength[String](4)) and
        (__ \ 'fields).read[Seq[String]] and
        (__ \ 'excludeContacts).readNullable[Boolean]
      )(VerifyRequest.apply _)

      validateFuture(request.body, reads) {
        vr =>
          // there needs to be at least one field
          vr.fields.isEmpty match {
            case true => Future(resBadRequest("at least one element in fields required"))
            case false =>
              val cameoId = if (vr.fields.contains("cameoId")) Some(vr.search) else None
              val displayName = if (vr.fields.contains("displayName")) Some(vr.search) else None

              Identity.search(cameoId, displayName).map {
                list =>
                  // todo: filter directly in mongo search
                  val filtered = list.filter(identity => {
                    val matchesContact: Boolean = vr.excludeContacts match {
                      case Some(true) => request.identity.contacts.exists(_.identityId.equals(identity.id))
                      case _          => false
                    }
                    !request.identity.id.equals(identity.id) && !matchesContact
                  })
                  resOK(filtered.map { i => i.toPublicSummaryJson })
              }
          }
      }
  }

  def addPublicKey() = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, PublicKey.createReads) {
        publicKey =>
          request.identity.addPublicKey(publicKey).map {
            case false => resServerError("unable to add")
            case true  => resOK(publicKey.toJson)
          }
      }
  }

  def editPublicKey(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, PublicKeyUpdate.format) {
        pku =>
          request.identity.editPublicKey(new MongoId(id), pku).map {
            case false => resServerError("not updated")
            case true  => resOK("updated")
          }
      }
  }

  def deletePublicKey(id: String) = AuthAction().async {
    request =>
      request.identity.deletePublicKey(new MongoId(id)).map {
        case false => resServerError("unable to delete")
        case true  => resOK("deleted")
      }
  }

}
