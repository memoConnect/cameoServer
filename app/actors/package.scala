import actors.testActors.SendSmsTestActor
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

  lazy val eventRouter: ActorRef = {
    val props = RoundRobinPool(5).props(Props[EventActor])
    Akka.system.actorOf(props, "event_router")
  }
  
  lazy val notificationRouter: ActorRef = {
    val props = RoundRobinPool(5).props(Props[NotificationActor])
    Akka.system.actorOf(props, "notification_router")
  }

}
