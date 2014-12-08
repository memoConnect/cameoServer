package controllers

import events.BroadcastEvent
import helper.ResultHelper._
import models.{ EventSubscription, MongoId }
import play.api.libs.json.{ JsObject, Json }
import play.api.mvc.Result
import services.AuthenticationActions.AuthAction
import services.AuthenticationActions
import traits.ExtendedController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 09.05.14
 * Time: 13:42
 */
object EventController extends ExtendedController {

  def newSubscription() = AuthAction(allowExternal = true).async(parse.tolerantJson) {
    request =>

      EventSubscription.checkAndCreate(request.identity.id, (request.body \ "secret").asOpt[String]).map {
        case None =>
          resBadRequest("max number of subscription reached")
        case Some(eventSubscription) => resOk(eventSubscription.toJson)
      }
  }

  def getSubscription(id: String) = AuthAction(allowExternal = true).async {
    request =>
      EventSubscription.findAndClear(MongoId(id), request.identity.id).flatMap {
        case None =>
          // create new event subscription
          EventSubscription.checkAndCreate(request.identity.id).map {
            case None                    => resKo()
            case Some(eventSubscription) => resKo(Json.obj("subscriptionId" -> eventSubscription.id.toJson))
          }
        case Some(subscription) => Future(resOk(subscription.toJson))
      }
  }

  case class EventBroadcastRequest(data: JsObject, name: String)

  object EventBroadcastRequest { implicit val format = Json.format[EventBroadcastRequest] }

  def broadcastEvent() = AuthAction()(parse.tolerantJson) {
    request =>
      validate(request.body, EventBroadcastRequest.format) {
        ebr =>
          actors.eventRouter ! BroadcastEvent(request.identity.id, ebr.name, ebr.data, request.identity)
          resOk("event send")
      }
  }

  def allowedRemoteEvents: Seq[String] =
    Seq(
      "authenticationRequest:start",
      "authenticationRequest:key-response",
      "authenticationRequest:key-request",
      "authenticationRequest:verified",
      "authenticationRequest:cancel"
    )

  def remoteBroadcastEvent(id: String) = AuthAction(includeContacts = true)(parse.tolerantJson) {
    request =>
      validate(request.body, EventBroadcastRequest.format) {
        ebr =>
          def sendEvent: Result = {
            val event = BroadcastEvent(new MongoId(id), ebr.name, ebr.data, request.identity)
            actors.eventRouter ! event
            resOk("event send")
          }

          // check if this identity is our own
          request.identity.id.id.equals(id) match {
            case true => sendEvent
            case false =>
              // check if remote identity is in contacts
              request.identity.contacts.exists(_.identityId.id.equals(id)) match {
                case false => resNotFound("identity in contacts")
                case true =>
                  // check if event name is in whitelist
                  allowedRemoteEvents.contains(ebr.name) match {
                    case false => resBadRequest("event not allowed")
                    case true  => sendEvent
                  }
              }
          }
      }
  }
}
