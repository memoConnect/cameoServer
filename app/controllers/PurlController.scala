package controllers

import helper.ResultHelper._
import models._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.{ JsObject, Json }
import play.api.mvc.{ Action, Result }
import traits.ExtendedController

import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 1/09/13
 * Time: 4:30 PM
 */

object PurlController extends ExtendedController {

  /**
   * Actions
   */
  def getPurl(id: String, offset: Int = 0, limit: Int = 0) = Action.async {
    request =>
      def externalUserResponse(identity: Identity, purl: Purl): Future[Result] = {
        // check if we need to generate a new token
        val token = identity.tokens.headOption.getOrElse {
          val t = Token.createDefault()
          identity.addToken(t)
          t
        }
        //get conversation
        Conversation.findByMessageId(purl.messageId, limit, offset).map {
          case None => resNotFound("conversation")
          case Some(conversation) =>
            conversation.hasMemberResult(identity.id) {
              val res: JsObject =
                Json.obj("conversation" -> conversation.toJson) ++
                  Json.obj("identity" -> identity.toPrivateJson) ++
                  Json.obj("token" -> token.id.toJson)
              resOk(res)
            }
        }
      }

      def getPurlWithToken(purl: Purl, token: String): Future[Result] = {
        // get identity behind purl
        Identity.find(purl.identityId).flatMap {
          case None => Future(resNotFound("purl"))
          case Some(identity) =>
            // check if the identity has an account
            identity.accountId match {
              case None => externalUserResponse(identity, purl)
              case Some(a) =>
                // purl belongs to a registered user, check we have the right token
                identity.tokens.exists(_.id.id.equals(token)) match {
                  case false => Future(resUnauthorized("This purl belongs to a different identity"))
                  case true =>
                    // get conversation
                    Conversation.findByMessageId(purl.messageId, limit, offset).map {
                      case None => resNotFound("conversation")
                      case Some(conversation) =>
                        conversation.hasMemberResult(identity.id) {
                          // return result
                          val res: JsObject =
                            Json.obj("conversation" -> conversation.toJson) ++
                              Json.obj("identity" -> identity.toPrivateJson)
                          resOk(res)
                        }
                    }
                }
            }
        }
      }

      def getPurlWithoutToken(purl: Purl): Future[Result] = {
        // check if identity is an external user
        Identity.find(purl.identityId).flatMap {
          case None => Future(resNotFound("identity"))
          case Some(identity) =>
            identity.accountId match {
              case Some(a) => Future(resUnauthorized("This purl belongs to a registered user, pleas supply token"))
              case None    => externalUserResponse(identity, purl)
            }
        }
      }

      Purl.find(id).flatMap {
        case None => Future(resNotFound("purl"))
        case Some(purl) =>

          // check if we have an authentication header
          request.headers.get("Authorization") match {
            case None        => getPurlWithoutToken(purl)
            case Some(token) => getPurlWithToken(purl, token)
          }
      }
  }
}