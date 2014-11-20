package controllers

import actors.ExternalMessage
import events.{ConversationUpdate, ConversationNew}
import helper.OutputLimits
import helper.ResultHelper._
import models._
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import play.api.mvc.Result
import services.AuthenticationActions.AuthAction
import services.AuthenticationActions
import traits.ExtendedController

import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 7:51 PM
 */
object ConversationController extends ExtendedController {

  def createConversation = AuthAction(includeContacts = true).async(parse.tolerantJson) {
    request =>
      {
        def addRecipients(conversation: Conversation): Either[Conversation, Result] = {
          (request.body \ "recipients").asOpt[Seq[String]] match {
            case None => Left(conversation)
            case Some(recipientIds) =>
              checkRecipients(recipientIds, conversation, request.identity) match {
                case None             => Right(resKo("Invalid recipients. Not in contact book."))
                case Some(recipients) => Left(conversation.copy(recipients = recipients ++ conversation.recipients))
              }
          }
        }

        def addMessages(conversation: Conversation): Either[Conversation, Result] = {
          (request.body \ "messages").asOpt[JsArray] match {
            case None => Left(conversation)
            case Some(js) =>
              js.validate(Reads.seq(Message.createReads(request.identity.id))).map {
                messages =>
                  // send notification for last message only
                  messages.lastOption match {
                    case None          => // do nothing
                    case Some(message) => actors.externalMessageRouter ! ExternalMessage(message, conversation.id, conversation.recipients, conversation.subject.getOrElse(""))
                  }
                  Left(conversation.copy(messages = messages))
              }.recoverTotal {
                error => Right(resBadRequest(JsError.toFlatJson(error)))
              }
          }
        }

        def insertConversation(conversation: Conversation): Future[Result] = {
          Conversation.col.insert(conversation).map {
            le =>
              // send conversation:new event to all recipients
              conversation.recipients.foreach {
                recipient =>
                  actors.eventRouter ! ConversationNew(recipient.identityId, conversation)
              }
              resOk(conversation.toJson(identityId = request.identity.id))
          }
        }

        validateFuture[Conversation](request.body, Conversation.createReads(Recipient.create(request.identity.id))) {
          conversation =>
            addRecipients(conversation) match {
              case Right(result) => Future(result)
              case Left(withRecipients) =>
                addMessages(withRecipients) match {
                  case Right(result) => Future(result)
                  case Left(withMessages) =>
                    insertConversation(withMessages)
                }
            }
        }
      }
  }

  def getConversation(id: String, offset: Int, limit: Int, keyId: List[String], timeLimit: Long) = AuthAction(allowExternal = true).async {
    request =>
      // check if a timeLimit is specified
      val futureConversation = if (timeLimit > 0) {
        Conversation.findWithTimeLimit(id, timeLimit)
      } else {
        Conversation.find(id, limit, offset)
      }

      futureConversation.map {
        case None => resNotFound("conversation")
        case Some(c) => c.hasMemberResult(request.identity.id) {
          val res = c.toJson(request.identity.id, Some(keyId))
          resOk(res)
        }
      }
  }

  def getConversationMessages(id: String, offset: Int, limit: Int) = AuthAction(allowExternal = true).async {
    request =>
      Conversation.find(id, limit, offset).map {
        case None => resNotFound("conversation")
        case Some(c) => c.hasMemberResult(request.identity.id) {
          resOk(c.toMessageJson)
        }
      }
  }

  def updateConversation(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>
      ConversationModelUpdate.validateRequest(request.body) {
        update =>
          Conversation.find(id, -1, 0).flatMap {
            case None => Future(resNotFound("conversation"))
            case Some(c) => c.hasMemberFutureResult(request.identity.id) {
              Conversation.update(c.id, update).map {
                case false => resServerError("could not update")
                case true  => resOk("updated")
              }
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

  def addRecipients(id: String) = AuthAction(includeContacts = true).async(parse.tolerantJson) {
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
      Conversation.find(id, 1, 0).map {
        case None => resNotFound("conversation")
        case Some(c) => c.hasMemberResult(request.identity.id) {
          resOk(c.toSummaryJson(request.identity.id, keyId))
        }
      }
  }

  def getConversations(offset: Int, limit: Int, keyId: List[String]) = AuthAction().async {
    request =>
      Conversation.findByIdentityId(request.identity.id).map {
        list =>
          // TODO: this can be done more efficiently with the aggregation framework in mongo
          val sorted = list.sortBy(_.lastUpdated).reverse
          val limited = OutputLimits.applyLimits(sorted, offset, limit)
          val res = Json.obj(
            "conversations" -> limited.map(_.toSummaryJson(request.identity.id, keyId)),
            "numberOfConversations" -> list.length
          )
          resOk(res)
      }
  }

  def addAePassphrases(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>
      Conversation.find(MongoId(id), -1, 0).flatMap {
        case None => Future(resNotFound("conversation"))
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

  def markMessageRead(id: String, messageId: String) = AuthAction(getAccount = true).async {
    request =>

      if(request.account.isDefined && request.account.get.userSettings.enableUnreadMessages) {
        Conversation.find(MongoId(id), -1, 0).flatMap {
          case None => Future(resNotFound("conversation"))
          case Some(conversation) =>
            conversation.hasMemberFutureResult(request.identity.id) {
              // mark message read in database
              conversation.markMessageRead(request.identity.id, MongoId(messageId)).map {
                case false => resBadRequest("unable to update")
                case true =>
                  // send event
                  val unreadMessages = conversation.messages.filterNot(_.fromIdentityId.equals(request.identity.id)).indexWhere(_.id.equals(MongoId(messageId)))
                  actors.eventRouter ! ConversationUpdate(request.identity.id, conversation.id, Json.obj("unreadMessages" -> unreadMessages))
                  resOk("updated")
              }
            }
        }
      } else {
        Logger.error("User with disabled unread message submitted message read. IdentityId: "+ request.identity.id.id)
        Future(resBadRequest("unread messages are disabled"))
      }
  }
}