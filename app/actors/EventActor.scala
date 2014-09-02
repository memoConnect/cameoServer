package actors

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
      EventSubscription.storeEvent(msg.sendToIdentity, msg.toEvent)
    case msg: EventDefinition =>
      EventSubscription.storeEvent(msg.sendToIdentity, msg.toEvent)
  }
}
