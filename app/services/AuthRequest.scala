package services

import constants.Authentication._
import helper.ResultHelper._
import models.{ Account, Identity, MongoId, TwoFactorToken }
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 11/5/13
 * Time: 5:57 PM
 */
class AuthRequest[A](val identity: Identity, val account: Option[Account], request: Request[A]) extends WrappedRequest[A](request)
class TwoFactorAuthRequest[A](val identity: Identity, request: Request[A]) extends WrappedRequest[A](request)

object AuthenticationActions {

  def accessDenied[A]: Request[A] => Future[Result] = {
    request => Future(resUnauthorized(REQUEST_ACCESS_DENIED))
  }

  def AuthAction(allowExternal: Boolean = false, includeContacts: Boolean = false, getAccount: Boolean = false, nonAuthBlock: Option[Request[Any] => Future[Result]] = None) =
    new ActionBuilder[AuthRequest] {
      def invokeBlock[A](request: Request[A], block: AuthRequest[A] => Future[Result]) =
        doAuthAction(allowExternal, includeContacts, getAccount, request, block, nonAuthBlock.getOrElse(accessDenied))
    }

  def TwoFactorAuthAction(includeContacts: Boolean = false, getAccount: Boolean = false) = new ActionBuilder[TwoFactorAuthRequest] {
    def invokeBlock[A](request: Request[A], block: TwoFactorAuthRequest[A] => Future[Result]) = {
      // do normal Auth and then try to elevate it to two factor auth
      doAuthAction(allowExternal = false, includeContacts, getAccount, request, elevate(block), accessDenied)
    }
  }

  def doAuthAction[A](allowExternal: Boolean, includeContacts: Boolean, getAccount: Boolean, request: Request[A], block: AuthRequest[A] => Future[Result], nonAuthBlock: Request[A] => Future[Result]) = {

    def processToken(token: String): Future[Result] = {
      Identity.findByToken(new MongoId(token), includeContacts).flatMap {
        case None => nonAuthBlock(request)
        case Some(identity) =>
          // check if external users are allowed
          (allowExternal, identity.accountId) match {
            case (false, None) => nonAuthBlock(request)
            case _ =>
              // check if we need to get the account
              val futureAccount: Future[Option[Account]] = (getAccount, identity.accountId) match {
                case (true, Some(accountId)) => Account.find(accountId)
                case _                       => Future(None)
              }
              futureAccount.flatMap {
                account => block(new AuthRequest[A](identity, account, request))
              }
          }
      }
    }

    // check if a token is in the header
    request.headers.get(REQUEST_TOKEN_HEADER_KEY) match {
      case Some(token) => processToken(token)
      case None =>
        // check if the token is a query parameter
        request.queryString.get("token") match {
          case Some(Seq(token)) => processToken(token)
          case None             => nonAuthBlock(request)
        }
    }
  }

  // elevate authLevel from regular to twoFactor, return error if not authorized
  def elevate[A](block: TwoFactorAuthRequest[A] => Future[Result]): (AuthRequest[A] => Future[Result]) = {
    authRequest =>
      authRequest.headers.get(REQUEST_TWO_FACTOR_TOKEN_HEADER_KEY) match {
        case None => Future.successful(resUnauthorized(REQUEST_TWO_FACTOR_TOKEN_MISSING, twoFactorRequired = true))
        case Some(twoFactorTokenId) =>
          // try to find token
          TwoFactorToken.find(twoFactorTokenId).flatMap {
            case None => Future(resUnauthorized(REQUEST_TWO_FACTOR_ACCESS_DENIED, twoFactorRequired = true))
            case Some(twoFactorToken) =>
              // make sure that identities match
              authRequest.identity.id.equals(twoFactorToken.identityId) match {
                case false => Future(resUnauthorized(REQUEST_TWO_FACTOR_ACCESS_DENIED, twoFactorRequired = true))
                case true  => block(new TwoFactorAuthRequest[A](authRequest.identity, authRequest))
              }
          }
      }
  }
}