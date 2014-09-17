package actors

import akka.actor.Actor
import models._
import play.api.Logger
import play.api.i18n.Lang
import services.{ EventDefinition, LocalizationMessages, PushEvent }

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
      // get push devices
      // todo: find a way to avoid these two db calls
      Identity.find(msg.sendToIdentity).map {
        case None => // do nothing
        case Some(identity) =>
          identity.accountId match {
            case None => // do nothing
            case Some(accountId) =>
              Account.find(accountId).map {
                case None => // do nothing
                case Some(account) =>
//                  account.pushDevices.foreach {
//                    pushDevice =>
//                      try {
//                        val language = Lang(pushDevice.language)
//                        val prefix = identity.getDisplayName + ": "
//                        val message = prefix + LocalizationMessages.get(msg.localizationKey, language, msg.localizationVariables)
//                        val pushNotification = PushNotification(message, pushDevice.deviceId.toString)
//                        pushNotificationRouter ! pushNotification
//                      } catch {
//                        case e: RuntimeException if e.getMessage.contains("Unrecognized language") =>
//                          Logger.error("Could not parse language id: " + pushDevice.language, e)
//                        case e: RuntimeException =>
//                          Logger.error("Error getting translated Message", e)
//                      }
//                  }
              }
          }
      }

      EventSubscription.storeEvent(msg.sendToIdentity, msg.toEvent)
    case msg: EventDefinition =>
      EventSubscription.storeEvent(msg.sendToIdentity, msg.toEvent)
  }
}
