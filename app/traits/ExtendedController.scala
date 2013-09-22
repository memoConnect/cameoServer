package traits

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.mvc._
import play.modules.reactivemongo.MongoController
import play.api.libs.concurrent.Akka
import akka.actor.Props
import actors.{SendKolibriActor, SendSMSActor, SendMailActor, SendMessageActor}
import play.api.Play.current
import models.Token


/**
 * User: Björn Reimer
 * Date: 5/21/13
 * Time: 6:53 PM
 */
/**
 * Several Helper functions for interaction with MongoDB *
 */
trait ExtendedController extends Controller with MongoController with MongoHelper {


  /**
   * Generate Result
   */
  def resOK() = Json.obj("res" -> "OK")

  def resOK(data: JsValue) = Json.obj("res" -> "OK") ++ Json.obj("data" -> data)

  def resKO(error: JsValue) = Json.obj("res" -> "KO") ++ Json.obj("error" -> error)

  def resOK(data: String) = Json.obj("res" -> "OK") ++ Json.obj("data" -> data)

  def resKO(error: String) = Json.obj("res" -> "KO") ++ Json.obj("error" -> error)

  /**
   * Authentication
   */
  case class AuthenticatedRequest[T](user: String, private val request: Request[T]) extends WrappedRequest(request)

  // checks if the token belongs to the given userclass
  def authenticate[T](token: String, requireAdminRights: Boolean = false, hasToBeRegistered: Boolean = true)(
    f: (Token, Request[T]) => Result
    )(implicit request: Request[T]): Result = {
    Async {
      // check if the token is in the database
      val futureToken = tokenCollection.find(Json.obj("token" -> token)).one[Token]
      futureToken.map {
        case None => Unauthorized(resKO("Invalid Token"))
        case Some(tokenObject) => {

          // we found the token, check if it has the proper rights
          if (requireAdminRights && !tokenObject.isAdmin) {
            Unauthorized(resKO("This action requires admin privileges"))
          }
          else {
            // check if this a registered user or just coming in via a purl
            tokenObject.username match {
              case Some(username) => f(tokenObject, request)
              case None => {
                if (hasToBeRegistered) {
                  Unauthorized(resKO("This action is only available to registered users"))
                } else {
                  f(tokenObject, request)
                }
              }
            }
          }
        }
      }
    }
  }


  def authenticatePOST(requireAdminRights: Boolean = false, hasToBeRegistered: Boolean = true)
                      (f: (Token, Request[JsValue]) => Result) = {
    Action(parse.tolerantJson) {
      implicit request => {
        (request.body \ "token").asOpt[String] match {
          case None => Unauthorized(resKO("No token"))
          case Some(m) => authenticate[JsValue](m, requireAdminRights, hasToBeRegistered)(f)
        }
      }
    }
  }

  def authenticateGET[T](
                          token: String,
                          requireAdminRights: Boolean = false,
                          bodyParser: BodyParser[T] = parse.empty,
                          hasToBeRegistered: Boolean = true
                          )(
                          f: (Token, Request[T]) => Result
                          ) = {
    Action(bodyParser) {
      implicit request => authenticate[T](token, requireAdminRights)(f)
    }
  }

}
