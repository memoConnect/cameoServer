package actors

import akka.actor.Actor
import models._
import play.api.Logger
import play.api.i18n.Lang
import services.{PushdConnector, EventDefinition, LocalizationMessages, PushEvent}

import scala.concurrent.ExecutionContext.Implicits.global
/**
 * User: Björn Reimer
 * Date: 12.05.14
 * Time: 13:28
 */

class EventActor extends Actor {

  def receive = {
    case event: EventDefinition with PushEvent =>
      pushNotificationRouter ! PushNotification(event)
      EventSubscription.storeEvent(event.sendToIdentity, event.toEvent)
    case event: EventDefinition =>
      EventSubscription.storeEvent(event.sendToIdentity, event.toEvent)
  }
}
