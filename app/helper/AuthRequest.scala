package helper

import play.api.mvc._
import scala.concurrent.{ ExecutionContext, Future }
import ExecutionContext.Implicits.global
import models.{ MongoId, Identity, Token }
import helper.ResultHelper._

/**
 * User: Björn Reimer
 * Date: 11/5/13
 * Time: 5:57 PM
 */
class AuthRequest[A](val identity: Identity, request: Request[A]) extends WrappedRequest[A](request)

object AuthAction extends ActionBuilder[AuthRequest] {

  val REQUEST_TOKEN = "token"
  val REQUEST_TOKEN_MISSING = "no token"
  val REQUEST_ACCESS_DENIED = "not allowed"
  val EMPTY_USER = ""

  def invokeBlock[A](request: Request[A], block: (AuthRequest[A]) => Future[SimpleResult]) = {
    // check if a token is passed
    request.getQueryString(REQUEST_TOKEN) match {
      case None => Future.successful(Results.Unauthorized(resKO(REQUEST_TOKEN_MISSING)))
      case Some(tokenId) => {
        Token.find(new MongoId(tokenId)).flatMap {
          case None => Future.successful(Results.Unauthorized(resKO(REQUEST_ACCESS_DENIED)))
          case Some(token) => {
            Identity.find(token.identityId).flatMap {
              case None           => Future.successful(Results.Unauthorized(resKO(REQUEST_ACCESS_DENIED)))
              case Some(identity) => block(new AuthRequest[A](identity, request))
            }
          }
        }
      }
    }
  }
}
