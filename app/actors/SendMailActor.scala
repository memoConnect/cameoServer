package actors

import akka.actor.Actor
import play.api.Logger

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 8:01 PM
 */
class SendMailActor extends Actor {

  def receive = {
    case (from: String, to: String, subject: String, body: String) => {



      Logger.info("SendMailActor: Sending email to " + to + " from " + from + " with subject \'" + subject + "\'")
    }
  }

}
