import actors.testActors.SendSmsTestActor
import akka.actor.Props
import play.api.Play
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
      Props[SendSmsTestActor]
    } else {
      Props[SendSmsActor]
    }
  }

}
