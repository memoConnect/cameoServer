package controllers

import traits.ExtendedController
import models.{ MongoId, Conversation }
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._
import helper.AuthAction
import play.api.libs.json.JsError
import helper.ResultHelper._
import scala.Some

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 7:51 PM
 */
object ConversationController extends ExtendedController {

  def createConversation = AuthAction(parse.tolerantJson) {
    request =>
      {
        validate[Conversation](request.body, Conversation.createReads) {
          conversation =>
            {
              Conversation.col.insert(conversation)
              request.identity.addConversation(conversation.id)
              resOK(conversation.toJson)
            }
        }
      }
  }

  def getConversation(id: String, offset: Int, limit: Int) =
    AuthAction.async {
      request =>
        Conversation.find(new MongoId(id)).flatMap {
          case None    => Future.successful(NotFound(resKO("conversation not found")))
          case Some(c) => c.toJsonWithDisplayNamesResult(offset, limit)
        }
    }

  def addRecipients(id: String) =
    // TODO: confirm that all recipients exist
    AuthAction.async(parse.tolerantJson) {
      request =>
        (request.body \ "recipients").validate[Seq[String]].map {
          recipients =>
            Conversation.find(new MongoId(id)).flatMap {
              case None => Future.successful(NotFound(resKO("invalid id")))
              case Some(c) => c.addRecipients(recipients.map(new MongoId(_))).map {
                lastError =>
                  if (lastError.ok) {
                    resOK("updated")
                  }
                  else {
                    BadRequest(resKO("updated failed"))
                  }
              }
            }
        }.recoverTotal(error => Future.successful(resBadRequest(JsError.toFlatJson(error).toString())))
    }

  //
  //
  //  def getConversationSummary(conversationId: String,
  //                             token: String) =
  //
  //    AuthAction.async(parse.tolerantJson) {
  //      implicit request => checkIfAllowed(conversationId) {
  //        conversation =>
  //          Ok(resOK(Conversation.toJsonCustomWrites(conversation, Conversation.summaryWrites)))
  //      }
  //    }
  //
  //
  //  def getConversations(token: String, offset: Int, limit: Int) = AuthAction.async(parse.tolerantJson) {
  //    implicit request =>
  //      implicit val outputLimits = OutputLimits(offset, limit)
  //      // for registered users only
  //      if (!request.token.username.isDefined) {
  //        Future.successful(Unauthorized(resKO("No user account")))
  //      } else {
  //        User.find(request.token.username.get).flatMap {
  //          case None => Future.successful(Unauthorized(resKO("user not found")))
  //          case Some(user) =>
  //            Conversation.getFromList(user.conversations).map {
  //              conversations =>
  //                val jsArray = Conversation.toSortedJsonArray(conversations, Conversation.summaryWrites)
  //                val res = Json.obj(
  //                  "numberOfConversations" -> conversations.size,
  //                  "conversations" -> jsArray
  //                )
  //                Ok(resOK(res))
  //            }
  //        }
  //      }
  //  }
}