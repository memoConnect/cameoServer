package controllers

import traits.ExtendedController
import models._
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._
import helper.OutputLimits
import helper.CmActions.AuthAction
import play.api.libs.json._
import helper.ResultHelper._

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 7:51 PM
 */
object ConversationController extends ExtendedController {

  def createConversation = AuthAction().async(parse.tolerantJson) {
    request => {
        validateFuture[Conversation](request.body, Conversation.createReads) {
          c =>
            {
              // add creator of conversation to recipients
              val withCreator = c.copy(recipients = c.recipients :+ Recipient.create(request.identity.id))
              Conversation.col.insert(withCreator)
              withCreator.toJsonWithIdentitiesResult
            }
        }
      }
  }

  def getConversation(id: String, offset: Int, limit: Int) = AuthAction(allowExternal = true).async {
    request =>
      Conversation.find(id, limit, offset).flatMap {
        case None => Future.successful(resNotFound("conversation"))
        case Some(c) => c.hasMemberFutureResult(request.identity.id) {
          c.toJsonWithIdentitiesResult
        }
      }
  }

  def addRecipients(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>

      Conversation.find(new MongoId(id)).flatMap {
        case None => Future.successful(resNotFound("conversation"))
        case Some(c) =>

          c.hasMemberFutureResult(request.identity.id) {

            validateFuture[Seq[String]](request.body \ "recipients", Reads.seq[String]) {
              recipientIds =>
                {
                  // check if one of the identities is already a member of this conversation
                  recipientIds.forall(id => !c.hasMember(new MongoId(id))) match {
                    case false => Future(resKO("At least one identity is already a member of this conversation"))
                    case true =>

                      // check if all recipients are in the users address book
                      recipientIds.forall(recipient => request.identity.contacts.exists(_.identityId.id.equals(recipient))) match {
                        case false => Future(resKO("At least one identity is not a contact"))
                        case true =>

                          // check if all recipients exist
                          val maybeIdentities = Future.sequence(recipientIds.map(Identity.find))
                          val futureResult: Future[Boolean] = maybeIdentities.map {
                            i => i.forall(_.isDefined)
                          }
                          futureResult.flatMap {
                            case false => Future(resBadRequest("At least one identityId is invalid"))
                            case true =>
                              c.addRecipients(recipientIds.map(Recipient.create)).map {
                                case true  => resOK("updated")
                                case false => resServerError("update failed")
                              }

                          }
                      }
                  }
                }
            }
          }
      }
  }

  def deleteRecipient(id: String, rid: String) = AuthAction().async {
    request =>
      Conversation.find(id).flatMap {
        case None => Future(resNotFound("conversation"))
        case Some(c) => c.hasMemberFutureResult(request.identity.id) {
          c.deleteRecipient(Recipient.create(new MongoId(rid))).map {
            case false => resNotFound("recipient")
            case true  => resOK()
          }
        }
      }
  }

  def getConversationSummary(id: String) = AuthAction(allowExternal = true).async {
    request =>
      Conversation.find(id).flatMap {
        case None => Future(resNotFound("conversation"))
        case Some(c) => c.hasMemberFutureResult(request.identity.id) {
          c.toSummaryJsonWithRecipientsResult
        }
      }
  }

  def getConversations(offset: Int, limit: Int) = AuthAction().async {
    request =>
      Conversation.findByIdentityId(request.identity.id).flatMap {
        list =>
          // TODO: this can be done more efficiently with the aggregation framework in mongo
          val sorted = list.sortBy(_.lastUpdated).reverse
          val limited = OutputLimits.applyLimits(sorted, offset, limit)
          val futureJson = Future.sequence(limited.map(_.toSummaryJsonWithRecipients))
          futureJson.map {
            json =>
              val res = Json.obj("conversations" -> json, "numberOfConversations" -> list.length)
              resOK(res)
          }
      }
  }

  def updateConversation(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, ConversationUpdate.format) {
        cu =>
          Conversation.find(id).flatMap {
            case None => Future(resNotFound("conversation"))
            case Some(c) => c.hasMemberFutureResult(request.identity.id) {
              c.update(cu).map {
                case false => resServerError("could not update")
                case true  => resOK("updated")
              }
            }
          }
      }
  }

  def setEncryptedPassphraseList(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>
      Conversation.find(id).flatMap {
        case None => Future(resNotFound("conversation"))
        case Some(c) => c.hasMemberFutureResult(request.identity.id) {
          val list: JsValue = (request.body \ "encryptedPassphraseList").asOpt[JsValue].getOrElse(JsArray())
          validateFuture(list, Reads.seq(EncryptedPassphrase.createReads)) {
            list =>
              c.setEncPassList(list).map {
                case false => resServerError("unable to update")
                case true  => resOK("updated")
              }
          }
        }
      }
  }

  case class AddPassCaptcha(passCaptcha: String)

  object AddPassCaptcha {
    implicit val format = Json.format[AddPassCaptcha]
  }

  def addPassCaptcha(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, AddPassCaptcha.format) {
        apc =>
          // check if conversation exists
          Conversation.find(id).flatMap {
            case None => Future(resNotFound("conversation"))
            case Some(conversation) => conversation.hasMemberFutureResult(request.identity.id) {

              // check if fileId exists
              FileMeta.find(apc.passCaptcha).map {
                case None => resNotFound("file")
                case Some(f) =>
                  conversation.setPassCaptcha(new MongoId(apc.passCaptcha))
                  resOK()
              }
            }
          }
      }
  }
}