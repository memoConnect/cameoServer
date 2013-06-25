package traits

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.mvc._
import play.modules.reactivemongo.MongoController
import play.api.libs.concurrent.Akka
import akka.actor.Props
import actors.SendMessageActor
import play.api.Play.current
import play.api.Logger
import reactivemongo.api.gridfs.GridFS
import scala.concurrent.{Future, ExecutionContext}
import reactivemongo.bson.BSONValue


/**
 * User: Björn Reimer
 * Date: 5/21/13
 * Time: 6:53 PM
 */
/**
 * Several Helper functions for interaction with MongoDB *
 */
trait ExtendedController extends Controller with MongoController with JsonTransformer with MongoHelper with ModelHelper {

  lazy val sendMessageActor = Akka.system.actorOf(Props[SendMessageActor], name = "sendMessage")

  /**
   * Authentication
   */
  case class AuthenticatedRequest[T](user: String, private val request: Request[T]) extends WrappedRequest(request)

  // checks if the token belongs to the given userclass
  def authenticate[T](token: String, requireAdminRights: Boolean = false)(f: (String,
    Request[T]) => Result)(implicit request: Request[T]): Result = {
    Async {
      // check if the token is in the database
      val futureUser = tokenCollection.find(Json.obj("token" -> token)).one[JsValue]
      futureUser.map {
        case None => Unauthorized(resKO("Invalid Token"))
        case Some(js) => {
          (js \ "username").asOpt[String] match {
            case None => {
              Logger.error("Unable to get user from token")
              InternalServerError("Error getting token")
            }
            case Some(username: String) => {
              // we found the token, check if it has the proper rights
              if (requireAdminRights) {
                (js \ "isAdmin").asOpt[Boolean] match {
                  case None => Unauthorized(resKO("This action requires admin privileges"))
                  case Some(b: Boolean) => {
                    if (!b) Unauthorized(resKO("This action requires admin privileges"))
                    else f(username, request)
                  }
                }
              } else
                f(username, request)
            }
          }
        }
      }
    }
  }


  def authenticatePOST(maxLength: Int = 128 * 1024, requireAdminRights: Boolean = false)(f: (String, Request[JsValue]) => Result) = {
    Action(parse.tolerantJson(maxLength)) {
      implicit request => {
        (request.body \ "token").asOpt[String] match {
          case None => Unauthorized(resKO("No token"))
          case Some(m) => authenticate[JsValue](m, requireAdminRights)(f)
        }
      }
    }
  }

  def authenticateGET[T](token: String, requireAdminRights: Boolean = false, bodyParser: BodyParser[T] = parse.empty)(f: (String,
    Request[T]) => Result) = {
    Action(bodyParser) {
      implicit request => authenticate[T](token, requireAdminRights)(f)
    }
  }

}
