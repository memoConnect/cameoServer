package controllers

import play.api.libs.json.{JsArray, Json}
import traits.{OutputLimits, ExtendedController}
import models.{User, Conversation}
import scala.concurrent.Future
import play.api.Logger

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 7:51 PM
 */
object ConversationController extends ExtendedController {

  def getConversation(conversationId: String, token: String, offset: Int, limit: Int) = authenticateGET(token) {
    (username, request) =>
      Async {
        implicit val outputLimits = OutputLimits(offset, limit)
        Conversation.find(conversationId).map {
          case None => NotFound(resKO("The conversation does not exist"))
          case Some(conversation) =>  Ok(resOK(Conversation.toJson(conversation)))
        }
      }
  }

  def getConversations(token: String, offset: Int, limit: Int) = authenticateGET(token) {
    (username, request) =>
      def getConversations(ids: Seq[String]): Future[List[Conversation]] = {
        val query = Json.obj("$or" -> ids.map(s => Json.obj("conversationId" -> s)))
        conversationCollection.find(query).sort(Json.obj("lastUpdated" -> -1)).cursor[Conversation].toList
      }

      val futureConversations = for {
        user <- User.find(username)
        conversations <- user match {
          case None => Future(Seq())
          case Some(u) => getConversations(u.conversations)
        }
      } yield conversations

      Async {
        futureConversations.map {
          c => {
            implicit val outputLimits = OutputLimits(offset, limit)
            val res = Conversation.toSortedJsonArray(c)
            Ok(resOK(res))
          }
        }
      }
  }
}