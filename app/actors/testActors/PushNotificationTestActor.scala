package actors.testActors

import actors.{PushNotification, Mail}
import akka.actor.Actor
import helper.TestValueStore
import play.api.libs.json.{JsObject, Json}

/**
 * User: Björn Reimer
 * Date: 3/13/14
 * Time: 6:08 PM
 */
class PushNotificationTestActor extends Actor {

  def receive = {
    case msg: PushNotification =>
      TestValueStore.addValue("push", Json.toJson(msg).as[JsObject])
  }
}
