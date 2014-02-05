package actors

import play.api.Play.current
import akka.actor.{ Props, Actor }
import traits.MongoHelper
import models.{ Message }
import play.api.Logger
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._

/**
 * User: Björn Reimer
 * Date: 9/19/13
 * Time: 7:23 PM
 */
class NotificationActor extends Actor with MongoHelper {

  def receive = {
    // notify user that a message has been received
    case (username: String, message: Message) => {
      //      User.find(username).map {
      //        case None => Logger.error("NotificationActor: could not find user " + username)
      //        case Some(user) => {
      //          sendSMSActor !(user, message)
      //          Logger.debug("NotificationActor: send notifcation to " + user.username)
      //        }
      //      }
    }
  }
}
