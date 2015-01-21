import actors.testActors.{ PushNotificationTestActor, SendMailTestActor, SendSmsTestActor }
import akka.actor.{ ActorRef, Props }
import akka.routing.RoundRobinPool
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.{ Logger, Play }

/**
 * User: Björn Reimer
 * Date: 3/14/14
 * Time: 10:22 AM
 */
package object actors {

  // define different actor for tests
  def SendSmsActorProps: Props = {
    if (Play.isTest) {
      Logger.debug("Using SMS Test Actor")
      Props[SendSmsTestActor]
    } else {
      Props[SendSmsActor]
    }
  }

  def SendMailActorProps: Props = {
    if (Play.isTest) {
      Logger.debug("Using Mail Test Actor")
      Props[SendMailTestActor]
    } else {
      Props[SendMailActor]
    }
  }

  def PushNotificationActorProps: Props = {
    if (Play.isTest) {
      Logger.debug("Using Push Notification Test Actor")
      Props[PushNotificationTestActor]
    } else {
      Props[PushNotificationActor]
    }
  }

  lazy val eventRouter: ActorRef = {
    val props = new RoundRobinPool(5).props(Props[EventActor])
    Akka.system.actorOf(props, "event_router")
  }

  lazy val externalMessageRouter: ActorRef = {
    val props = new RoundRobinPool(5).props(Props[ExternalMessageActor])
    Akka.system.actorOf(props, "external_messages_router")
  }

  lazy val pushNotificationRouter: ActorRef = {
    val props = new RoundRobinPool(1).props(PushNotificationActorProps)
    Akka.system.actorOf(props, "push_notification_router")
  }

  lazy val verificationRouter: ActorRef = {
    val props = new RoundRobinPool(5).props(Props[VerificationActor])
    Akka.system.actorOf(props, "verification_router")
  }

  lazy val resetPasswordRouter: ActorRef = {
    val props = new RoundRobinPool(5).props(Props[ResetPasswordActor])
    Akka.system.actorOf(props, "reset_password_router")
  }
}
