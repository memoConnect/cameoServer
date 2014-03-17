package controllers

import play.api.libs.json.{ JsObject, Json }
import traits.ExtendedController
import models._
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import helper.AuthAction
import scala.concurrent.Future
import helper.ResultHelper._
import play.api.mvc.{ SimpleResult, Result, Action }
import scala.Some
import play.api.mvc.SimpleResult

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

      def getPurlWithToken(purl: Purl, token: String): Future[SimpleResult] = {
        // check if token exists
        Identity.findToken(new MongoId(token)).flatMap {
          case None => Future(resNotFound("token"))
          case Some(identity) =>
            // check if the identityId match
            identity.id.equals(purl.identityId) match {
              case false => Future(resUnauthorized("This purl belongs to a different identity"))
              case true => {
                // get conversation
                Conversation.findByMessageId(purl.messageId).flatMap {
                  case None => Future(resNotFound("conversation"))
                  case Some(conversation) =>
                    // return result
                    conversation.toJsonWithIdentities(offset, limit).map {
                      js =>
                        val res: JsObject =
                          Json.obj("conversation" -> js) ++
                            Json.obj("identity" -> identity.toPrivateJson)
                        resOK(res)
                    }
                }
              }
            }
        }
      }

      def getPurlWithoutToken(purl: Purl): Future[SimpleResult] = {
        // check if identity is an external user
        Identity.find(purl.identityId).flatMap {
          case None => Future(resNotFound("identity"))
          case Some(identity) =>
            identity.accountId match {
              case Some(a) => Future(resUnauthorized("This purl belong to a registered user, pleas supply token"))
              case None =>
                // check if we need to generate a new token
                val token = identity.tokens.headOption.getOrElse {
                  val t = Token.create()
                  identity.addToken(t)
                  t
                }
                //get conversation
                Conversation.findByMessageId(purl.messageId).flatMap {
                  case None => Future(resNotFound("conversation"))
                  case Some(conversation) =>
                    // return result
                    conversation.toJsonWithIdentities(offset, limit).map {
                      js =>
                        val res: JsObject =
                          Json.obj("conversation" -> js) ++
                          Json.obj("identity" -> identity.toPrivateJson) ++
                          Json.obj("token" -> token.id.toJson)

                        resOK(res)
                    }
                }
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