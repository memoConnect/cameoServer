package controllers

import play.api.libs.json._
import play.api.libs.json.Reads._

import helper.IdHelper
import scala.None
import traits.ExtendedController
import play.api.Logger
import models.{Conversation, Recipient, Message}
import java.util.Date


/**
 * User: Björn Reimer
 * Date: 5/21/13
 * Time: 7:08 PM
 */
object MessageController extends ExtendedController {

  /**
   * Actions
   */
  def sendMessage = authenticatePOST() {
    (username, request) =>
      val jsBody: JsValue = request.body

      jsBody.validate[Message](Message.inputReads).map {
        message => {
          // check if we need to create a new conversation
          val conversationId: String = message.conversationId match {
            case None => {
              //create new conversation
              val conversation: Conversation = new Conversation(
                IdHelper.generateConversationId(),
                new Date,
                new Date,
                Seq(),
                Seq())

              conversationCollection.insert(conversation)
              conversation.conversationId
            }
            case Some(c) => c
          }
          val recipients: Seq[Recipient] = message.recipients.getOrElse(Seq())

          val newMessage = message.copy(sendStatus = Json.obj("state" -> "queued"), recipients = None,
            conversationId = Some(conversationId), from = username)

          // add message and recipient to conversation
          Async {
            val query = Json.obj("conversationId" -> conversationId)
            val set = Json.obj("$push" -> Json.obj("messages" -> newMessage)) ++ Json.obj("$pushAll" -> Json.obj
              ("recipients" -> recipients))

            conversationCollection.update(query, set).map {
              lastError =>
                if (!lastError.updatedExisting) {
                  BadRequest(resKO("The conversation does not exist"))
                } else if (lastError.ok) {
                  Ok(resOK(Json.toJson(newMessage)(Message.outputWrites)))
                } else {
                  Logger.debug("Mongo Error: " + lastError.stringify + " Query: " + query.toString + " Set: " + set
                    .toString)
                  InternalServerError(resKO("DB Error"))
                }
            }
          }
        }
      }.recoverTotal(e => BadRequest(JsError.toFlatJson(e)))
  }

  def getMessage(messageId: String, token: String) = authenticateGET(token) {
    (username, request) =>
      Async {
        val query = Json.obj("messages.messageId" -> messageId)
        val filter = Json.obj("messages.$" -> 1)

        conversationCollection.find(query, filter).one[JsObject].map {
          case None => NotFound(resKO("messageId not found"))
          case Some(js) => (js \ "messages")(0).asOpt[Message] match {
            case None => NotFound(resKO("messageId not found"))
            case Some(m) => Ok(resOK(Json.toJson(m)(Message.outputWrites)))
          }
        }
      }
  }
}
