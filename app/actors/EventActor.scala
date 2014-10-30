package actors

import akka.actor.Actor
import models._
import services.{ EventDefinition, PushEvent }
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
