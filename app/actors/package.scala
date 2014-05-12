import actors.testActors.SendSmsTestActor
import akka.actor.{ActorRef, Props}
import akka.routing.{RoundRobinRouter, SmallestMailboxRouter}
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

  def eventRouter: ActorRef = {

    val props = RoundRobinRouter(5).props(Props[RequestActor]), "requestRouter"


    system.actorOf()
  }

}
