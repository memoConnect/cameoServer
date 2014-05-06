package controllers

import traits.ExtendedController

import helper.CmActions.AuthAction
import scala.concurrent.{ Await, ExecutionContext, Future }
import helper.ResultHelper._
import play.api.libs.json._
import ExecutionContext.Implicits.global
import play.api.libs.json.JsObject
import play.api.{ Logger, Play }
import play.api.mvc._
import play.api.http.Writeable
import play.api.libs.iteratee.{ Iteratee, Input }
import play.api.libs.json.JsObject
import scala.Some
import scala.concurrent.duration._
import play.api.Play.current
import play.core.parsers.FormUrlEncodedParser

/**
 * User: Björn Reimer
 * Date: 29.04.14
 * Time: 15:20
 */
object CallStackController extends ExtendedController {

  // define available http methods
  trait HttpMethod
  case object GET extends HttpMethod
  case object POST extends HttpMethod
  case object PUT extends HttpMethod
  case object DELETE extends HttpMethod
  case object OTHER extends HttpMethod

  case class Call(method: HttpMethod,
                  path: String,
                  data: Option[JsObject])

  object Call {
    implicit val methodReads: Reads[HttpMethod] = Reads {
      _.validate[String].map {
        method =>
          method.toLowerCase match {
            case "get"    => GET
            case "post"   => POST
            case "put"    => PUT
            case "delete" => DELETE
            case _        => OTHER
          }
      }
    }

    implicit val callReads: Reads[Call] = Json.reads[Call]
  }

  case class CallStack(requests: Seq[Call])

  object CallStack {
    implicit val reads = Json.reads[CallStack]
  }

  def processCallStack() = Action.async(parse.tolerantJson) {
    request =>
      {
        validateFuture[CallStack](request.body, CallStack.reads) {
          callStack =>

            // check length of call stack
            callStack.requests.length <= Play.configuration.getInt("callstack.length.max").get match {
              case false => Future(resBadRequest(Json.obj("maxLength" -> Play.configuration.getInt("callstack.length.max").get)))
              case true =>
                val responses = callStack.requests.map {
                  call =>
                    // todo: dont hardcode /api/v1
                    val path = "/api/v1" + call.path.split('?').take(1).mkString

                    // create new request
                    val rawQuery = call.path.split('?').drop(1).mkString
                    val query = FormUrlEncodedParser.parse(rawQuery)
                    val newRequestHeader: RequestHeader = request.copy(uri = path, path = path, method = call.method.toString, queryString = query)

                    // get request handler from router
                    val handler: Option[Handler] = Play.current.routes.get.handlerFor(newRequestHeader)

                    // execute request, feed body using default writable for json
                    def doRequest(implicit w: Writeable[JsObject]): Option[Future[SimpleResult]] = {
                      handler.flatMap {
                        case a: EssentialAction => Some(
                          Play.current.global.doFilter(a)(newRequestHeader)
                            .feed(Input.El(w.transform(call.data.getOrElse(Json.obj()))))
                            .flatMap(_.run)
                        )
                        case _ => None
                      }
                    }

                    doRequest match {
                      case None => Future(Json.obj("status" -> 404, "body" -> Json.obj()))
                      case Some(futureResponse) =>
                        futureResponse.map(response => {
                          val array = Await.result(response.body |>>> Iteratee.consume[Array[Byte]](), 1.minute)
                          Json.obj("body" -> Json.parse(new String(array, "utf-8"))) ++
                            Json.obj("status" -> response.header.status)
                        })
                    }
                }

                Future.sequence(responses).map {
                  list => resOK(Json.obj("responses" -> list))
                }
            }
        }
      }
  }
}
