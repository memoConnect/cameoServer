package controllers

import play.api.libs.json.{JsError, JsObject, Json}
import traits.ExtendedController
import models.Conversation

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 7:51 PM
 */
object ConversationController extends ExtendedController{

  def getConversation(conversationId: String, token: String) = authenticateGET(token) {
    (username, request) =>
      Async {
        val query = Json.obj("conversationId" -> conversationId)

        conversationCollection.find(query).one[Conversation].map {
          case None => NotFound(resKO("The conversation does not exist"))
          case Some(conversation) => Ok(resOK(Json.toJson(conversation)(Conversation.outputWrites)))
        }
      }
  }
}
