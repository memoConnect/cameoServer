import akka.actor.Props
import play.api.libs.concurrent.Akka
import play.api.Play.current

/**
 * User: Björn Reimer
 * Date: 10/14/13
 * Time: 3:10 PM
 */
package object actors {

  lazy val notificationActor = Akka.system.actorOf(Props[NotificationActor], name = "Notification")
  lazy val sendMessageActor = Akka.system.actorOf(Props[SendMessageActor], name = "sendMessage")
  lazy val sendMailActor = Akka.system.actorOf(Props[SendMailActor], name = "sendMail")
  lazy val sendSMSActor = Akka.system.actorOf(Props[SendSMSActor], name = "sendSMS")
  lazy val sendKolibriActor = Akka.system.actorOf(Props[SendKolibriActor], name = "sendKolibri")


}
