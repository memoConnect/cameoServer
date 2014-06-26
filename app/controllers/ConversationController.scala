package controllers

import traits.ExtendedController
import models._
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._
import helper.OutputLimits
import helper.CmActions.AuthAction
import play.api.libs.json._
import helper.ResultHelper._
import play.api.mvc.Result
import scala.Some
import play.api.mvc.Result

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 7:51 PM
 */
object ConversationController extends ExtendedController {

  def createConversation = AuthAction().async(parse.tolerantJson) {
    request =>
      {
        def insertConversation(conversation: Conversation): Future[Result] = {
          Conversation.col.insert(conversation).map {
            le => resOk(conversation.toJson)
          }
        }

        validateFuture[ConversationUpdate](request.body, ConversationUpdate.createReads) {
          cu =>
            val conversation = Conversation.create(cu.subject, Seq(Recipient.create(request.identity.id)), cu.passCaptcha, cu.aePassphraseList, cu.sePassphrase, cu.keyTransmission)

            // check if there are recipients
            (request.body \ "recipients").asOpt[Seq[String]] match {
              case None => insertConversation(conversation)
              case Some(recipientIds) =>
                checkRecipients(recipientIds, conversation, request.identity) match {
                  case None => Future(resBadRequest("Invalid recipients. Not in contact book."))
                  case Some(recipients) =>
                    val withRecipients = conversation.copy(recipients = recipients ++ conversation.recipients)
                    insertConversation(withRecipients)
                }
            }
        }
      }

  }

  def getConversation(id: String, offset: Int, limit: Int, keyId: List[String]) = AuthAction(allowExternal = true).async {
    request =>
      Conversation.find(id, limit, offset).flatMap {
        case None => Future(resNotFound("conversation"))
        case Some(c) => c.hasMemberFutureResult(request.identity.id) {
          c.getMissingPassphrases.map {
            missingPasshrases =>
              resOk(c.toJsonWithKey(keyId) ++ Json.obj("missingAePassphrase" -> missingPasshrases))
          }
        }
      }
  }

  def checkRecipients(recipientIds: Seq[String], conversation: Conversation, identity: Identity): Option[Seq[Recipient]] = {
    // remove all recipients that are already a member of this conversation and the sender himself
    val filtered = recipientIds.filterNot(id => conversation.recipients.exists(_.identityId.id.equals(id)) || id.equals(identity.id.id))

    // check if all recipients are in the users address book
    filtered.forall(recipient => identity.contacts.exists(_.identityId.id.equals(recipient))) match {
      case false => None
      case true  => Some(filtered.map(Recipient.create))
    }
  }

  def addRecipients(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>
      Conversation.find(new MongoId(id), -1, 0).flatMap {
        case None => Future.successful(resNotFound("conversation"))
        case Some(conversation) =>
          conversation.hasMemberFutureResult(request.identity.id) {
            validateFuture[Seq[String]](request.body \ "recipients", Reads.seq[String]) {
              recipientIds =>
                checkRecipients(recipientIds, conversation, request.identity) match {
                  case Some(recipients) =>
                    conversation.addRecipients(recipients).map {
                      case true  => resOk("updated")
                      case false => resServerError("update failed")
                    }
                  case None => Future(resKo("invalid recipient list"))
                }
            }
          }
      }
  }

  def deleteRecipient(id: String, rid: String) = AuthAction().async {
    request =>
      Conversation.find(id, -1, 0).flatMap {
        case None => Future(resNotFound("conversation"))
        case Some(c) => c.hasMemberFutureResult(request.identity.id) {
          c.deleteRecipient(new MongoId(rid)).map {
            case false => resNotFound("recipient")
            case true  => resOk()
          }
        }
      }
  }

  def getConversationSummary(id: String, keyId: List[String]) = AuthAction(allowExternal = true).async {
    request =>
      Conversation.find(id, -1, 0).flatMap {
        case None => Future(resNotFound("conversation"))
        case Some(c) => c.hasMemberFutureResult(request.identity.id) {
          c.toSummaryJsonWithKey(keyId).map(resOk(_))
        }
      }
  }

  def getConversations(offset: Int, limit: Int, keyId: List[String]) = AuthAction().async {
    request =>
      Conversation.findByIdentityId(request.identity.id).flatMap {
        list =>
          // TODO: this can be done more efficiently with the aggregation framework in mongo
          val sorted = list.sortBy(_.lastUpdated).reverse
          val limited = OutputLimits.applyLimits(sorted, offset, limit)
          val futureJson = Future.sequence(limited.map(_.toSummaryJsonWithKey(keyId)))
          futureJson.map {
            json =>
              val res = Json.obj("conversations" -> json, "numberOfConversations" -> list.length)
              resOk(res)
          }
      }
  }

  def updateConversation(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, ConversationUpdate.createReads) {
        cu =>
          Conversation.find(id, -1, 0).flatMap {
            case None => Future(resNotFound("conversation"))
            case Some(c) => c.hasMemberFutureResult(request.identity.id) {
              c.update(cu).map {
                case false => resServerError("could not update")
                case true  => resOk("updated")
              }
            }
          }
      }
  }

  def addAePassphrase(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>
      Conversation.find(new MongoId(id), -1, 0).flatMap {
        case None => Future.successful(resNotFound("conversation"))
        case Some(conversation) =>
          conversation.hasMemberFutureResult(request.identity.id) {
            validateFuture(request.body \ "aePassphraseList", Reads.seq(EncryptedPassphrase.createReads)) {
              conversation.addAePassphrases(_).map {
                case true  => resOk("updated")
                case false => resServerError("unable to update")
              }
            }
          }
      }
  }
}