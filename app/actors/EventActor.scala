package actors

import akka.actor.Actor
import models.Message
import play.api.Logger

/**
 * User: Björn Reimer
 * Date: 12.05.14
 * Time: 13:28
 */

case class NewMessage(conversationId: String, message: Message)


class EventActor extends Actor {

  def receive() = {
    case NewMessage(conversationId, message) =>
      Logger.info("new message!")
  }

}
