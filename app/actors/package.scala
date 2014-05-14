import actors.testActors.SendSmsTestActor
import akka.actor.{ ActorRef, Props }
import akka.routing.{ RoundRobinRouter, SmallestMailboxRouter }
import play.api.libs.concurrent.Akka
import play.api.{ Logger, Play }
import play.api.Play.current

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
    val props = Props[EventActor].withRouter(RoundRobinRouter(nrOfInstances = 5))
    Akka.system.actorOf(props, "event_router")
  }

}
