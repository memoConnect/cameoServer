package actors

import akka.actor.Actor
import models._
import play.api.Logger
import play.api.i18n.Lang
import services.{PushdConnector, EventDefinition, LocalizationMessages, PushEvent}

import scala.concurrent.ExecutionContext.Implicits.global
/**
 * User: Björn Reimer
 * Date: 12.05.14
 * Time: 13:28
 */

class EventActor extends Actor {

  def receive = {
    case msg: EventDefinition with PushEvent =>
      // send push notification
      // todo: find a way to avoid this db call
      Identity.find(msg.sendToIdentity).map {
        case None => // do nothing
        case Some(identity) =>
           val titles = LocalizationMessages.getAll(msg.localizationKeyTitle, msg.localizationVariables)
           val content = LocalizationMessages.getAll(msg.localizationKeyMsg, msg.localizationVariables)
          val contentWithIdentity = content.mapValues{
            msg => identity.displayName.getOrElse(identity.cameoId) + ": " + msg
          }
          PushdConnector.sendEvent(identity.id.id, titles, contentWithIdentity)
      }

      EventSubscription.storeEvent(msg.sendToIdentity, msg.toEvent)
    case msg: EventDefinition =>
      EventSubscription.storeEvent(msg.sendToIdentity, msg.toEvent)
  }
}
