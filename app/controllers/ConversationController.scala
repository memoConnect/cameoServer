package controllers

import play.api.libs.json.Json
import traits.{OutputLimits, ExtendedController}
import models.{Token, User, Conversation}
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 7:51 PM
 */
object ConversationController extends ExtendedController {

  def getConversation(conversationId: String, token: String, offset: Int, limit: Int) = authenticateGET(token) {
    (tokenObject: Token, request) =>
      Async {
        implicit val outputLimits = OutputLimits(offset, limit)
        Conversation.find(conversationId).map {
          case None => NotFound(resKO("The conversation does not exist"))
          case Some(conversation) => Ok(resOK(Conversation.toJson(conversation)))
        }
      }
  }

  def getConversationSummary(conversationId: String, token: String) = authenticateGET(token) {
    (tokenObject: Token, request) =>
      Async {
        Conversation.find(conversationId).map {
          case None => NotFound(resKO("The conversation does not exist"))
          case Some(conversation) => Ok(resOK(Conversation.toJsonCustomWrites(conversation, Conversation.summaryWrites)))
        }
      }
  }

  def getConversations(token: String, offset: Int, limit: Int) = authenticateGET(token) {
    (tokenObject: Token, request) =>
      def getUserConversations(ids: Seq[String]): Future[List[Conversation]] = {
        val query = Json.obj("$or" -> ids.map(s => Json.obj("conversationId" -> s)))
        conversationCollection.find(query).sort(Json.obj("lastUpdated" -> -1)).cursor[Conversation].toList
      }

      val futureConversations = for {
        user <- User.find(tokenObject.username.get)
        conversations <- user match {
          case None => Future(Seq())
          case Some(u) => {
            if (u.conversations.length > 0) {
              getUserConversations(u.conversations)
            } else {
              Future(Seq())
            }
          }
        }
      } yield conversations

      Async {
        futureConversations.map {
          conversationList => {
            implicit val outputLimits = OutputLimits(offset, limit)
            val array = Conversation.toSortedJsonArray(conversationList, Conversation.summaryWrites)

            val res = Json.obj(
              "numberOfConversations" -> conversationList.size,
              "conversations" -> array
            )

            Ok(resOK(res))
          }
        }
      }
  }
}