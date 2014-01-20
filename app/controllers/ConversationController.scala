package controllers

import traits.{OutputLimits, ExtendedController}
import models.{MongoId, Purl, Conversation}
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.SimpleResult
import helper.{AuthAction, AuthRequest}
import services.Authentication.UserClass
import services.Authentication
import play.api.libs.json.Json


/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 7:51 PM
 */
object ConversationController extends ExtendedController {

////  def checkIfAllowed[A](conversationId: String)(action: (Conversation) => SimpleResult)(implicit request: AuthRequest[A]): Future[SimpleResult] = {
////
////    // Check if the user has the proper rights
////    val userClass: UserClass = Authentication.getUserClass(request.token.userClass.getOrElse(AuthAction.EMPTY_USER))
////
////    if (!userClass.accessIfMember) {
////      Future.successful(Unauthorized)
////    } else {
////      // check if the user is a member of this conversation
////      Conversation.find(conversationId).flatMap {
////        case None => Future.successful(NotFound(resKO("The conversation does not exist")))
////        case Some(conversation) => {
////          for {
////          // get user
////            user <- {
////              request.token.username match {
////                case Some(username) => Future.successful(username)
////                case None => {
////                  // get id from purl
////                  Purl.find(request.token.purl.get).map {
////                    case None => ""
////                    case Some(purl) => purl.name.get
////                  }
////                }
////              }
////            }
////            result <- {
////              if (Conversation.hasMember(conversation, user)) {
////                Future.successful(action(conversation))
////              } else {
////                Future.successful(Unauthorized)
////              }
////            }
////          } yield result
////        }
////      }
////    }
////  }
//
  def getConversation(id: String, offset: Int, limit: Int) =
    AuthAction.async {
      request =>
        Conversation.find(new MongoId(id)).map {
          case None => NotFound(resKO("conversation not found"))
          case Some(c) => Ok(resOK(c.toJson))
        }
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