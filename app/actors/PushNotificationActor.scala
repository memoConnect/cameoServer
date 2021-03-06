package actors

import akka.actor.Actor
import events.{ EventDefinition, PushEvent }
import models.Identity
import services.{ LocalizationMessages, PushdConnector }

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 02.09.14
 * Time: 15:11
 */

case class PushNotification(event: EventDefinition with PushEvent)

class PushNotificationActor extends Actor {
  def receive = {
    case PushNotification(event) =>
      // send push notification
      // todo: find a way to avoid this db call
      Identity.find(event.sendToIdentity).map {
        case None => // do nothing
        case Some(identity) =>
          val titles = LocalizationMessages.getAll(event.localizationKeyTitle, event.localizationVariables)
          val content = LocalizationMessages.getAll(event.localizationKeyMsg, event.localizationVariables)
          val contentWithIdentity = content.mapValues {
            msg => identity.displayName.getOrElse(identity.cameoId) + ": " + msg
          }
          PushdConnector.sendEvent(identity.id.id, titles, contentWithIdentity, event.context)
      }
  }
}
