package actors.testActors

import actors.PushNotification
import akka.actor.Actor
import helper.TestValueStore
import play.api.libs.json.{ JsObject, Json }

/**
 * User: Björn Reimer
 * Date: 3/13/14
 * Time: 6:08 PM
 */
class PushNotificationTestActor extends Actor {

  def receive = {
    case PushNotification(event) =>
      TestValueStore.addValue("push", Json.obj("sendToIdentity" -> event.sendToIdentity.id))
  }
}
