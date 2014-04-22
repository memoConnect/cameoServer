package helper

import play.api.mvc._
import scala.concurrent.{ ExecutionContext, Future }
import ExecutionContext.Implicits.global
import models.{ MongoId, Identity }
import helper.ResultHelper._
import constants.Authentication._

/**
 * User: Björn Reimer
 * Date: 11/5/13
 * Time: 5:57 PM
 */
class AuthRequest[A](val identity: Identity, request: Request[A]) extends WrappedRequest[A](request)

object AuthRequestHelper {

  def authAction(allowExternal: Boolean = false) = new ActionBuilder[AuthRequest] {
    def invokeBlock[A](request: Request[A], block: AuthRequest[A] => Future[SimpleResult]) =
      doAction(allowExternal, request, block)
  }

  def doAction[A](allowExternal: Boolean, request: Request[A], block: AuthRequest[A] => Future[SimpleResult]) = {
    // check if a token is passed
    request.headers.get(REQUEST_TOKEN_HEADER_KEY) match {
      case None => Future.successful(resUnauthorized(REQUEST_TOKEN_MISSING))
      case Some(token) => {
        Identity.findByToken(new MongoId(token)).flatMap {
          case None => Future.successful(resUnauthorized(REQUEST_ACCESS_DENIED))
          case Some(identity) =>
            // check if identity has account
            (allowExternal, identity.accountId) match {
              case (false, None) => Future.successful(resUnauthorized(REQUEST_ACCESS_DENIED))
              case _ => block(new AuthRequest[A](identity, request))
            }
        }
      }
    }
  }
}