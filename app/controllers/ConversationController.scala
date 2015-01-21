package controllers

import actors.ExternalMessage
import events.{ ConversationNew, ConversationUpdate }
import helper.OutputLimits
import helper.ResultHelper._
import models._
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.mvc.Result
import services.AuthenticationActions.AuthAction
import traits.ExtendedController
import play.api.libs.functional.syntax._

import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 7:51 PM
 */
object ConversationController extends ExtendedController {

  def createConversation = AuthAction(includeContacts = true, getAccount = true).async(parse.tolerantJson) {
    request =>
      {
        def addRecipients(conversation: Conversation): Either[Conversation, Result] = {

          def withRecipients(newRecipients: Seq[Recipient]): Either[Conversation, Result] = {
            checkRecipients(newRecipients, conversation, request.identity).left.map {
              checkedRecipients =>
                val allRecipients = conversation.recipients ++ checkedRecipients
                // make sure that the sender is a recipient
                val withSender = if (allRecipients.exists(_.identityId.equals(request.identity.id))) {
                  allRecipients
                } else {
                  allRecipients :+ Recipient.create(request.identity.id)
                }
                conversation.copy(recipients = withSender)
            }
          }

          // recipients can be either an array of strings or recipient objects
          (request.body \ "recipients").asOpt[JsValue] match {
            case None => Left(conversation.copy(recipients = Seq(Recipient.create(request.identity.id))))
            case Some(recipientsJs) if recipientsJs.asOpt[Seq[String]].isDefined =>
              val recipientIds = recipientsJs.as[Seq[String]]
              withRecipients(recipientIds.map(id => Recipient.create(MongoId(id))))
            case Some(recipientsJs) =>
              validateEither(recipientsJs, Reads.seq(Recipient.createReads)).left.flatMap {
                recipientsJs => withRecipients(recipientsJs)
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
                    case Some(message) => actors.externalMessageRouter ! ExternalMessage(message, conversation)
                  }
                  Left(conversation.copy(messages = messages))
              }.recoverTotal {
                error => Right(resBadRequest("invalid json", data = Some(JsError.toFlatJson(error))))
              }
          }
        }

        def insertConversation(conversation: Conversation): Future[Result] = {
          Conversation.insert(conversation).map {
            le =>
              // send conversation:new event to all recipients
              conversation.recipients.foreach {
                recipient =>
                  actors.eventRouter ! ConversationNew(recipient.identityId, conversation)
              }
              resOk(conversation.toJson(request.identity.id, request.account.map(_.userSettings)))
          }
        }

        validateFuture[Conversation](request.body, Conversation.createReads) {
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

  def getConversation(id: String, offset: Int, limit: Int, keyId: List[String], timeLimit: Long) = AuthAction(allowExternal = true, getAccount = true).async {
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
          val res = c.toJson(request.identity.id, request.account.map(_.userSettings), Some(keyId))
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
      ConversationModelUpdate.fromRequest(request.body) {
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

  def checkRecipients(recipients: Seq[Recipient], conversation: Conversation, sender: Identity): Either[Seq[Recipient], Result] = {
    // remove all recipients that are already a member of this conversation
    val filtered = recipients.diff(conversation.recipients)

    // check if all recipients are in the users address book or the sender
    filtered.forall(recipient => sender.contacts.exists(_.identityId.equals(recipient.identityId)) || sender.id.equals(recipient.identityId)) match {
      case false => Right(resBadRequest("Invalid recipients. Not in contact book."))
      case true  => Left(filtered)
    }
  }

  //  def addRecipients(id: String) = AuthAction(includeContacts = true).async(parse.tolerantJson) {
  //    request =>
  //      Conversation.find(new MongoId(id), -1, 0).flatMap {
  //        case None => Future.successful(resNotFound("conversation"))
  //        case Some(conversation) =>
  //          conversation.hasMemberFutureResult(request.identity.id) {
  //            validateFuture[Seq[String]](request.body \ "recipients", Reads.seq[String]) {
  //              recipientIds =>
  //                checkRecipients(recipientIds, conversation, request.identity) match {
  //                  case Some(recipients) =>
  //                    conversation.addRecipients(recipients).map {
  //                      case true  => resOk("updated")
  //                      case false => resServerError("update failed")
  //                    }
  //                  case None => Future(resKo("invalid recipient list"))
  //                }
  //            }
  //          }
  //      }
  //  }

  //  def deleteRecipient(id: String, rid: String) = AuthAction().async {
  //    request =>
  //      Conversation.find(id, -1, 0).flatMap {
  //        case None => Future(resNotFound("conversation"))
  //        case Some(c) => c.hasMemberFutureResult(request.identity.id) {
  //          c.deleteRecipient(new MongoId(rid)).map {
  //            case false => resNotFound("recipient")
  //            case true  => resOk("deleted")
  //          }
  //        }
  //      }
  //  }

  def getConversationSummary(id: String, keyId: List[String]) = AuthAction(allowExternal = true, getAccount = true).async {
    request =>
      Conversation.find(id, 1, 0).map {
        case None => resNotFound("conversation")
        case Some(c) => c.hasMemberResult(request.identity.id) {
          resOk(c.toSummaryJson(request.identity.id, request.account.map(_.userSettings), keyId))
        }
      }
  }

  def getConversations(offset: Int, limit: Int, keyId: List[String]) = AuthAction(getAccount = true).async {
    request =>
      Conversation.findByIdentityId(request.identity.id).map {
        list =>
          // TODO: this can be done more efficiently with the aggregation framework in mongo
          val sorted = list.sortBy(_.lastUpdated).reverse
          val limited = OutputLimits.applyLimits(sorted, offset, limit)
          val res = Json.obj(
            "conversations" -> limited.map(_.toSummaryJson(request.identity.id, request.account.map(_.userSettings), keyId)),
            "numberOfConversations" -> list.length
          )
          resOk(res)
      }
  }

  def addAePassphrases(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>
      Conversation.find(MongoId(id), 1, 0).flatMap {
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
      if (request.account.isDefined && request.account.get.userSettings.enableUnreadMessages) {
        // we assume that the message we are marking as read is in the last 10 messages, so we do not have to get all messages from the db.
        val messageLimit = 10

        Conversation.find(MongoId(id), messageLimit, 0).flatMap {
          case None => Future(resNotFound("conversation"))
          case Some(conversation) =>
            conversation.hasMemberFutureResult(request.identity.id) {
              val index = conversation.messages.indexWhere(_.id.equals(MongoId(messageId)))
              val stillUnread = if (index >= 0) index else messageLimit
              conversation.markMessageRead(request.identity.id, stillUnread).map {
                case false => resBadRequest("unable to update")
                case true =>
                  // send event
                  actors.eventRouter ! ConversationUpdate(request.identity.id, conversation.id, Json.obj("unreadMessages" -> stillUnread))
                  resOk("updated")
              }
            }
        }
      } else {
        Logger.error("User with disabled unread message submitted message read. IdentityId: " + request.identity.id.id)
        Future(resBadRequest("unread messages are disabled"))
      }
  }

  case class FindConversationRequest(search: String,
                                     foo: Boolean // needed because foo 
                                     )
  object FindConversationRequest {
    implicit val reads = (
      (__ \ 'search).read[String](minLength[String](3)) and
      Reads.pure(true)
    )(FindConversationRequest.apply _)
  }

  def findConversations(offset: Int, limit: Int, keyId: List[String]) = AuthAction(includeContacts = true, getAccount = true).async(parse.tolerantJson) {
    request =>
      validateFuture[FindConversationRequest](request.body, FindConversationRequest.reads) {
        fcr =>
          val searchTerm = fcr.search.toLowerCase

          request.identity.getContactIdentities.flatMap {
            identities =>
              // filter all identities that match the search term
              val matchingIdentities = (identities :+ request.identity).filter {
                i =>
                  i.cameoId.toLowerCase.contains(searchTerm) ||
                    i.displayName.exists(_.toLowerCase.contains(searchTerm))
              }

              // search conversations that match the subject or recipient
              Conversation.search(request.identity.id, Some(fcr.search), matchingIdentities.map(_.id)).map {
                conversations =>
                  val sorted = conversations.sortBy(_.lastUpdated).reverse
                  val limited = OutputLimits.applyLimits(sorted, offset, limit)
                  val res = Json.obj(
                    "conversations" -> limited.map(_.toSummaryJson(request.identity.id, request.account.map(_.userSettings), keyId)),
                    "numberOfMatches" -> conversations.length
                  )
                  resOk(res)
              }
          }
      }
  }

  def deleteOwnRecipient(id: String) = AuthAction().async {
    request =>
      Conversation.find(id).map{
        case None => resNotFound("conversation")
        case Some(conversation) =>
          conversation.hasMemberResult(request.identity.id) {
            // delete recipient
            conversation.deleteRecipient(request.identity.id)
            // delete his aepassphrases
            conversation.deleteAePassphrases(request.identity.publicKeys.map(_.id.toString))
            resOk("deleted")
          }
      }
  }
}