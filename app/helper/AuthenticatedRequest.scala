package helper

import play.api.mvc._
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global
import models.Token
import scala.util.parsing.json.JSONObject

/**
 * User: Björn Reimer
 * Date: 11/5/13
 * Time: 5:57 PM
 */


class AuthenticatedRequest[A](val token: String, request: Request[A]) extends WrappedRequest[A](request)

object Authenticated extends ActionBuilder[AuthenticatedRequest] {



  def invokeBlock[A](request: Request[A], block: (AuthenticatedRequest[A]) => Future[SimpleResult]) = {


    request.method match {
      case "POST" => Future.successful(Results.Forbidden("POST"))
      case _ => Future.successful(Results.Forbidden("REST"))
    }





  }

  // POST request have to have json body
  def invokeBlock(request: Request[JSONObject], block: (AuthenticatedRequest[JSONObject]) => Future[SimpleResult]) = {

    request.method match {
      case "POST" => authenticatePOST(request, block)
    }





  }

  def authenticatePOST[A](request: Request[JSONObject], block: (AuthenticatedRequest[JSONObject]) => Future[SimpleResult]) =  {

    val body: JSONObject = request.body;

    //

    ////
    ////      request match {
    ////        case r: Request[JSONObject] => block(new AuthenticatedRequest("username", request))
    ////        case _ => Future.successful(BadRequest("No Json"))
    ////      }
    //
    //
  }



}
