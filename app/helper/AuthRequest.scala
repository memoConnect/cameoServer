package helper

import play.api.mvc._
import scala.concurrent.{ ExecutionContext, Future }
import ExecutionContext.Implicits.global
import models.{ TwoFactorToken, MongoId, Identity, Token }
import helper.ResultHelper._
import constants.Authentication._

/**
 * User: Björn Reimer
 * Date: 11/5/13
 * Time: 5:57 PM
 */
class AuthRequest[A](val identity: Identity, request: Request[A]) extends WrappedRequest[A](request)

object AuthAction extends ActionBuilder[AuthRequest] {

  def invokeBlock[A](request: Request[A], block: AuthRequest[A] => Future[SimpleResult]) = {
    // check if a token is passed
    request.headers.get(REQUEST_TOKEN_HEADER_KEY) match {
      case None => Future.successful(resUnauthorized(REQUEST_TOKEN_MISSING))
      case Some(token) => {
        Identity.findToken(new MongoId(token)).flatMap {
          case None           => Future.successful(resUnauthorized(REQUEST_ACCESS_DENIED))
          case Some(identity) => block(new AuthRequest[A](identity, request))
        }
      }
    }
  }
}

class TwoFactorAuthRequest[A](val identity: Identity, request: Request[A]) extends WrappedRequest[A](request)

object TwoFactorAuthAction extends ActionBuilder[TwoFactorAuthRequest] {

  // elevate  authLevel from regular to twoFactor, return error if not authorized
  def elevate[A](block: TwoFactorAuthRequest[A] => Future[SimpleResult]): (AuthRequest[A] => Future[SimpleResult]) = {
    authRequest =>
      authRequest.headers.get(REQUEST_TWO_FACTOR_TOKEN_HEADER_KEY) match {
        case None => Future.successful(resUnauthorized(REQUEST_TWO_FACTOR_TOKEN_MISSING, twoFactorRequired = true))
        case Some(twoFactorTokenId) => {
          // try to find token
          TwoFactorToken.find(twoFactorTokenId).flatMap {
            case None => Future(resUnauthorized(REQUEST_TWO_FACTOR_ACCESS_DENIED, twoFactorRequired = true))
            case Some(twoFactorToken) => {
              // make sure that identities match
              authRequest.identity.id.equals(twoFactorToken.identityId) match {
                case false => Future(resUnauthorized(REQUEST_TWO_FACTOR_ACCESS_DENIED, twoFactorRequired = true))
                case true => block(new TwoFactorAuthRequest[A](authRequest.identity, authRequest))
              }
            }
          }
        }
      }
  }

  def invokeBlock[A](request: Request[A], block: TwoFactorAuthRequest[A] => Future[SimpleResult]) = {
    // do normal Auth an then try to elevate it to two factor auth
    val elevatedBlock = elevate(block)
    AuthAction.invokeBlock(request, elevatedBlock)
  }
}
