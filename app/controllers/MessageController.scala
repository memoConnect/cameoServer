package controllers

import play.api.libs.json._

import helper.IdHelper
import scala.None
import traits.ExtendedController
import play.api.Logger
import models.{User, Conversation, Recipient, Message}
import java.util.Date
import scala.concurrent.Future
import reactivemongo.core.commands.LastError


/**
 * User: Björn Reimer
 * Date: 5/21/13
 * Time: 7:08 PM
 */
object MessageController extends ExtendedController {

  /**
   * Helper
   */

  // check if we need to create a new conversation
  def makeSureConversationExists(implicit message: Message): Future[String] = {
    message.conversationId match {
      case None => {
        //create new conversation
        val conversation: Conversation = new Conversation(IdHelper.generateConversationId(), new Date,
          new Date, Seq(), Seq(), None)

        conversationCollection.insert(conversation).map {
          lastError => conversation.conversationId
        }
      }
      case Some(c) => Future(c)
    }
  }

  // get recipients from conversation and add those new from this message
  def createMessage(conversationId: String, username: String)(implicit message: Message): Future[Message] = {
    def newMessageWithRecipients(recipients: Seq[Recipient]): Message = {
      message.copy(recipients = Some(recipients), conversationId = Some(conversationId), from = username)
    }
    // get recipients from conversation
    Conversation.find(conversationId).map {
      case None => {
        Logger.error("Could not find conversation: " + conversationId)
        newMessageWithRecipients(message.recipients.getOrElse(Seq()))
      }
      case Some(c) => newMessageWithRecipients(c.recipients ++ message.recipients.getOrElse(Seq()))
    }
  }

  // add this conversation to the user object
  def addConversationToUser(conversationId: String, username: String): Future[Option[String]] = {
    //get the user
    User.find(username).map {
      case None => {
        None
      }
      case Some(u) => {
        // check if the user already has this conversation
        if (!u.conversations.contains(conversationId)) {
          // add it if he does not have it
          val query = Json.obj("username" -> username)
          val set = Json.obj("$addToSet" -> Json.obj("conversations" -> conversationId))
          Logger.debug("Added conversationId " + conversationId + " to user " + username)

          userCollection.update(query, set)
        }
        None
      }
    }
  }

  // add message to conversation
  def addMessageToConversation(conversationId: String, newMessage: Message)
                              (implicit message: Message): Future[LastError] = {
    val query = Json.obj("conversationId" -> conversationId)
    val set = Json.obj("$push" -> Json.obj("messages" -> newMessage))
    conversationCollection.update(query, set)
  }

  // add recipient(s) from message to conversation
  def addRecipientsToConversation(conversationId: String)(implicit message: Message): Future[LastError] = {
    val recipients = message.recipients.getOrElse(Seq())
    val query = Json.obj("conversationId" -> conversationId)
    val set = Json.obj("$pushAll" -> Json.obj("recipients" -> recipients))
    conversationCollection.update(query, set)
  }

  /**
   * Actions
   */
  def sendMessage = authenticatePOST(hasToBeRegistered = false) {
    (username, request) =>
      val jsBody: JsValue = request.body

      jsBody.validate[Message](Message.inputReads).map {
        implicit message => {

          // execute steps asynchronously
          val errors = for {
            conversationId <- makeSureConversationExists
            newMessage <- createMessage(conversationId, username)
            userRes <- addConversationToUser(conversationId, username)
            recipientError <- addRecipientsToConversation(conversationId)
            messageError <- addMessageToConversation(conversationId, newMessage)

          } yield (userRes.isEmpty && messageError.ok && recipientError.ok, newMessage)

          // check result and send response
          Async {
            errors.map {
              case (true, newMessage) => {
                sendMessageActor ! newMessage
                Ok(resOK(Message.toJson(newMessage)))
              }
              case (false, newMessage) => {
                Logger.error("Error sending message: " + Message.toJson(newMessage))
                InternalServerError(resKO("DB Error"))
              }
            }
          }
        }
      }.recoverTotal(e => BadRequest(resKO(JsError.toFlatJson(e))))
  }

  def getMessage(messageId: String, token: String) = authenticateGET(token) {
    (username, request) =>
      Async {
        val message = Message.find(messageId)
        message.map {
          option => option match {
            case None => NotFound(resKO("messageId not found"))
            case Some(m) => Ok(resOK(Message.toJson(m)))
          }
        }
      }
  }


}
