package controllers

import helper.AuthenticationActions.AuthAction
import helper.ResultHelper._
import models.{ EventSubscription, MongoId }
import play.api.{ Logger, Play }
import play.api.Play.current
import play.api.libs.json.{ JsObject, Json }
import play.api.mvc.Result
import services.BroadcastEvent
import traits.ExtendedController

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 09.05.14
 * Time: 13:42
 */
object EventController extends ExtendedController {

  def newSubscription() = AuthAction(allowExternal = true).async(parse.tolerantJson) {
    request =>
      // check if a secret is used to disable max subscription
      val limitEnabled: Boolean = Play.configuration.getString("events.subscription.debug.secret") match {
        case None             => true
        case Some("disabled") => true
        case Some(str) =>
          // check if there is a secret in the body
          (request.body \ "secret").asOpt[String] match {
            case Some(secret) if secret.equals(str) => false
            case _                                  => true
          }
      }

      // check if maximum number for this user is exceeded
      val max = Play.configuration.getInt("events.subscription.user.limit").get
      EventSubscription.countUserSubscriptions(request.identity.id).map {
        case i if limitEnabled && i >= max =>
          resBadRequest("max number of subscription reached")
        case _ =>
          val subscription = EventSubscription.create(request.identity.id)
          EventSubscription.col.insert(subscription)
          resOk(subscription.toJson)
      }
  }

  def getSubscription(id: String) = AuthAction(allowExternal = true).async {
    request =>
      EventSubscription.findAndClear(MongoId(id), request.identity.id).map {
        case None => resNotFound("subscription id")
        case Some(subscription) =>
          resOk(subscription.toJson)
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

  def remoteBroadcastEvent(id: String) = AuthAction()(parse.tolerantJson) {
    request =>
      validate(request.body, EventBroadcastRequest.format) {
        ebr =>
          def sendEvent: Result = {
            val event = BroadcastEvent(new MongoId(id), ebr.name, ebr.data, request.identity)
            Logger.debug("Remote event to " + id + ": " + event.toEvent.toJson)
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
