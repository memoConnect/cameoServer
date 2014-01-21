package controllers

import play.api.libs.json.Json
import traits.{ExtendedController}
import models.{Conversation, Purl}
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import helper.AuthAction
import scala.concurrent.Future
import helper.ResultHelper._


/**
 * User: Björn Reimer
 * Date: 1/09/13
 * Time: 4:30 PM
 */

object PurlController extends ExtendedController {


  /**
   * Actions
   */
//  def getPurl(purl: String, offset: Int, limit: Int, token: String) = AuthAction.async {
//    request =>
//    // define output limits for conversation
//      implicit val outputLimits = OutputLimits(offset, limit)
//
//      Purl.find(purl).flatMap {
//        case None => Future.successful(NotFound(resKO("The purl does not exist")))
//        case Some(purlObject) => {
//
//          // get the conversation
//          Conversation.find(purlObject.conversationId).flatMap {
//            case None => {
//              val er = "The conversation in the purl not exist"
//              Logger.error(er)
//              Future.successful(NotFound(resKO(er)))
//            }
//            case Some(conversation) => {
//              // check if the user behind this purl is registered or anon
//              if (purlObject.username.isDefined) {
//                // registered user, check if the token is right
//                models.Token.find(token).map {
//                  case None => {
//                    Unauthorized(resKO(Json.obj("context" -> Purl.toJson(purlObject)), "no/invalid token"))
//                  }
//                  case Some(tokenObject) => {
//                    if (tokenObject.username.getOrElse("invalid").equals(purlObject.username.get)) {
//                      val res = Json.obj("context" -> Purl.toJson(purlObject),
//                        "conversation" -> Conversation.toJson
//                          (conversation))
//                      Ok(resOK(res))
//                    } else {
//                      Unauthorized(resKO(Json.obj("context" -> Purl.toJson(purlObject)), "token not authorized"))
//                    }
//                  }
//                }
//              } else {
//                // anon user, we don't care about the token
//                val res = Json.obj("context" -> Purl.toJson(purlObject), "conversation" -> Conversation.toJson
//                  (conversation))
//                Future.successful(Ok(resOK(res)))
//              }
//            }
//          }
//        }
//      }
//  }
}
