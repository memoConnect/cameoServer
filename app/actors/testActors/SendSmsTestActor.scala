package actors.testActors

import akka.actor.Actor
import helper.TestValueStore
import models.SmsMessage
import play.api.libs.json.{ JsObject, Json }

/**
 * User: Björn Reimer
 * Date: 3/13/14
 * Time: 6:08 PM
 */
class SendSmsTestActor extends Actor {

  def receive = {
    case (sms: SmsMessage, tryCount: Int) =>
      TestValueStore.addValue("sms", Json.toJson(sms).as[JsObject])
  }
}
