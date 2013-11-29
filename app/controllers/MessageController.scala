package controllers

import play.api.libs.json._

import helper.IdHelper
import scala.None
import traits.ExtendedController
import play.api.Logger
import models._
import java.util.Date
import scala.concurrent.Future
import scala.Some
import play.api.libs.concurrent.Execution.Implicits._



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

  // create message object and add message to conversation
  def addMessageToConversation(conversationId: String, tokenObject: Token, recipients: Seq[Recipient])
                              (implicit message: Message): Future[Message] = {
    val from: Future[(String, String)] = tokenObject.username match {
      case None => {
        // get name from purl
        Purl.find(tokenObject.purl.get).map {
          purl => (purl.get.name.getOrElse("no Name"), purl.get.recipientId)
        }
      }
      case Some(username) =>
        Future(username, "") // TODO: find recipientId of user
    }

    from.map {
      case (name, id) => {
        val newMessage = message.copy(from = name, fromRecipientId = Some(id), conversationId = Some(conversationId),
          recipients = Some(recipients))
        Conversation.addMessage(newMessage)
        newMessage
      }
    }
  }

  // merges the new list of recipients into the existing avoiding duplication of Kolibri Users.
  def compareRecipients(oldRecipients: Seq[Recipient], newRecipients: Seq[Recipient]): Seq[Recipient] = {
    newRecipients match {
      case Nil => Nil
      case head :: rest => {
        if (head.messageType.equals("otherUser") && oldRecipients.exists(r => r.messageType.equals("otherUser") &&
          r.sendTo.equals(head.sendTo))) {
          compareRecipients(oldRecipients, rest)
        } else {
          head +: compareRecipients(oldRecipients, rest)
        }
      }
    }
  }

  // get recipients from conversations and add the ones from the message
  def addRecipientsToConversation(conversationId: String, tokenObject: Token)
                                 (implicit message: Message): Future[Seq[Recipient]] = {

    Conversation.find(conversationId).map {
      case None => Seq()
      case Some(c) => {
        val messageRecipients = tokenObject.username match {
          case None => message.recipients.getOrElse(Seq())
          case Some(u) => new Recipient(IdHelper.generateRecipientId(), tokenObject.username.get, "otherUser",
            tokenObject.username.get, None, None) +: message.recipients.getOrElse(Seq())
        }
        val newRecipients: Seq[Recipient] = compareRecipients(c.recipients, messageRecipients)

        // save new recipients to conversation
        val query = Json.obj("conversationId" -> conversationId)
        val set = Json.obj("$pushAll" -> Json.obj("recipients" -> newRecipients))
        conversationCollection.update(query, set) map {
          lastError => if (lastError.inError) {
            Logger.error("MessageController: Error saving recipients. " + lastError.stringify)
          }
        }
        newRecipients ++ c.recipients
      }
    }
  }

  /**
   * Actions
   */
  def sendMessage = authenticatePOST(hasToBeRegistered = false) {
    (tokenObject: Token, request) =>
      val jsBody: JsValue = request.body

      jsBody.validate[Message](Message.inputReads).map {
        implicit message => {

          // execute steps asynchronously
          val newMessage = for {
            conversationId <- makeSureConversationExists
            allRecipients <- addRecipientsToConversation(conversationId, tokenObject)
            newMessage <- addMessageToConversation(conversationId, tokenObject, allRecipients)

          } yield newMessage

          // send response
          Async {
            newMessage.map {
              m => {
                // add conversation to user if registered
                if (tokenObject.username.isDefined) {
                  User.addConversation(m.conversationId.get, tokenObject.username.get)
                }

                actors.sendMessageActor ! m

                Ok(resOK(Message.toJson(m)))
              }
            }
          }
        }
      }.recoverTotal(e => BadRequest(resKO(JsError.toFlatJson(e))))
  }

  def getMessage(messageId: String, token: String) = authenticateGET(token) {
    (tokenObject: Token, request) =>
      Async {
        val message = Message.find(messageId)
        message.map {
          case None => NotFound(resKO("messageId not found"))
          case Some(m) => Ok(resOK(Message.toJson(m)))
        }
      }
  }


}
