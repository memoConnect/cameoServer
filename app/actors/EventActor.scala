package actors

import actors.PushNotification
import akka.actor.Actor
import models._
import play.api.libs.json.{ JsObject, Json }
import services.{PushEvent, EventDefinition}

/**
 * User: Björn Reimer
 * Date: 12.05.14
 * Time: 13:28
 */

class EventActor extends Actor {

  def receive = {
    case msg: EventDefinition with PushEvent =>
      // send push notification
      val pushNotification = PushNotification(msg.getMessageText, "foo")
      pushNotificationRouter ! pushNotification

      EventSubscription.storeEvent(msg.sendToIdentity, msg.toEvent)
    case msg: EventDefinition =>
      EventSubscription.storeEvent(msg.sendToIdentity, msg.toEvent)
  }
}
