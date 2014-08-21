package actors.testActors

import actors.{ Mail, Sms }
import akka.actor.Actor
import helper.TestValueStore
import play.api.Logger
import play.api.libs.json.{ JsObject, Json }

/**
 * User: Björn Reimer
 * Date: 3/13/14
 * Time: 6:08 PM
 */
class SendMailTestActor extends Actor {

  def receive = {
    case mail: Mail =>
      TestValueStore.addValue("mail", Json.toJson(mail).as[JsObject])
  }
}
