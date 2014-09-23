package controllers

import helper.AuthenticationActions._
import helper.IdHelper
import helper.ResultHelper._
import models._
import play.api.Logger
import play.api.libs.json._
import play.api.mvc.Result
import services.{ UpdatedIdentity }
import traits.ExtendedController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 22.07.14
 * Time: 17:08
 */
object PublicKeyController extends ExtendedController {

  def isOwnKey(identity: Identity, id: String)(action: => Future[Result]): Future[Result] = {
    identity.publicKeys.exists(_.id.id.equals(id)) match {
      case false => Future(resNotFound("public key"))
      case true  => action
    }
  }

  def addPublicKey() = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, PublicKey.createReads) {
        publicKey =>
          val withId = publicKey.copy(id = IdHelper.generatePublicKeyId(publicKey.key))
          // check if this key has already been uploaded by this user
          request.identity.publicKeys.find(_.id.equals(withId.id)) match {
            case Some(existingKey) =>
              // update key
              val update = PublicKeyUpdate(withId.name)
              request.identity.editPublicKey(withId.id, update).map {
                case false => resBadRequest("unable to update")
                case true  => resOk(withId.toJson)
              }
            case None =>
              // check if this id already exists
              PublicKey.find(withId.id).flatMap {
                case Some(p) => Future(resBadRequest("key uploaded by someone else"))
                case None =>
                  request.identity.addPublicKey(withId).map {
                    case false => resServerError("unable to add")
                    case true =>
                      // send event to all people in address book
                      request.identity.contacts.foreach {
                        contact =>
                          actors.eventRouter ! UpdatedIdentity(contact.identityId, request.identity.id, Json.obj("publicKeys" -> Seq(withId.toJson)))
                      }
                      // send event to ourselves
                      actors.eventRouter ! UpdatedIdentity(request.identity.id, request.identity.id, Json.obj("publicKeys" -> Seq(withId.toJson)))
                      resOk(withId.toJson)
                  }
              }
          }
      }
  }

  def editPublicKey(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>
      isOwnKey(request.identity, id) {
        validateFuture(request.body, PublicKeyUpdate.format) {
          pku =>
            request.identity.editPublicKey(new MongoId(id), pku).map {
              case false => resServerError("not updated")
              case true  => resOk("updated")
            }
        }
      }
  }

  def deletePublicKey(id: String) = AuthAction().async {
    request =>
      isOwnKey(request.identity, id) {
        request.identity.deletePublicKey(new MongoId(id)).map {
          case false => resServerError("unable to delete")
          case true =>
            actors.eventRouter ! UpdatedIdentity(request.identity.id, request.identity.id, Json.obj("publicKeys" -> Seq(Json.obj("id" -> id, "deleted" -> true))))
            resOk("deleted")
        }
      }
  }

  def addSignature(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture[Signature](request.body, Signature.format) {
        signature =>
          // check who the public key belongs to
          request.identity.publicKeys.find(_.id.id.equals(id)) match {
            case Some(publicKey) =>
              // add to own public key
              request.identity.addSignatureToPublicKey(new MongoId(id), signature).map {
                case false => resBadRequest("could not add")
                case true  =>
                  val newPublicKey = publicKey.copy(signatures = publicKey.signatures :+ signature)
                  val event = UpdatedIdentity(request.identity.id, request.identity.id, Json.obj("publicKeys" -> Json.toJson(Seq(newPublicKey))))
                  actors.eventRouter ! event
                  resOk(signature.toJson)
              }
            case None =>
              request.identity.addPublicKeySignature(id, signature).map {
                case false => resBadRequest("could not add")
                case true  => resOk(signature.toJson)
              }
          }
      }
  }

  def deleteSignature(id: String, keyId: String) = AuthAction().async {
    request =>
      // check who the public key belongs to
      request.identity.publicKeys.exists(_.id.id.equals(id)) match {
        case true =>
          // add to own public key
          request.identity.deleteSignatureFromPublicKey(new MongoId(id), keyId).map {
            case false => resServerError("could not delete")
            case true  => resOk("deleted")
          }
        case false =>
          request.identity.deletePublicKeySignature(id).map {
            case false => resServerError("could not delete")
            case true  => resOk("deleted")
          }
      }
  }

  case class AePassphrase(conversationId: String, aePassphrase: String)
  object AePassphrase { implicit val format = Json.format[AePassphrase] }

  def getAePassphrases(id: String, newKeyId: String, limit: Int) = AuthAction().async {
    request =>

      val maybeLimit = limit match {
        case 0 => None
        case x => Some(x)
      }

      Conversation.getAePassphrases(request.identity.id, new MongoId(id), new MongoId(newKeyId), maybeLimit).map {
        list =>
          resOk(list.map(Json.toJson(_)))
      }
  }

  def addAePassphrases(id: String) = AuthAction()(parse.tolerantJson) {
    request =>
      validate[Seq[AePassphrase]](request.body, Reads.seq(AePassphrase.format)) {
        list =>
          list.foreach {
            aePassphrase =>
              Conversation.addAePassphrases(
                Seq(EncryptedPassphrase.create(id, aePassphrase.aePassphrase)),
                new MongoId(aePassphrase.conversationId)).map {
                  case false => Logger.error("error while adding aePassphrase to conversation " + aePassphrase.conversationId)
                  case true  => Logger.debug("updated")
                }
          }
          resOk("updated")
      }
  }

}
