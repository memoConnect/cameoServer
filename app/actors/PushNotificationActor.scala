package actors

import akka.actor.Actor
import com.puship.{ CoreApi, Credentials, PushipUtil }
import play.api.Play.current
import play.api.libs.json.Json
import play.api.{ Logger, Play }

/**
 * User: Björn Reimer
 * Date: 02.09.14
 * Time: 15:11
 */

case class PushNotification(message: String,
                            deviceId: String)
object PushNotification { implicit val format = Json.format[PushNotification] }

class PushNotificationActor extends Actor {
  def receive = {
    case PushNotification(message, deviceId) =>

      val username = Play.configuration.getString("pushIp.username")
      val password = Play.configuration.getString("pushIp.password")
      val appId = Play.configuration.getString("pushIp.appId")

      Logger.debug(username + ":" + password + ":" + appId)

      username.isEmpty || password.isEmpty || appId.isEmpty match {
        case true =>
          Logger.warn("No PushIp credentials")
        case false =>

          Logger.debug("Sending Push Notification to " + deviceId + " : " + message)

          val credentials: Credentials = new Credentials(username.get, password.get)
          val coreApi: CoreApi = new CoreApi(appId.get, credentials)
          //          coreApi.EnableDebug = true
          PushipUtil.SetTimeZone("Europe/Berlin")

          val javaMap = new java.util.HashMap[String, AnyRef]()
          javaMap.put("Message", message)

          val javaSet = new java.util.HashSet[String]()
          javaSet.add(deviceId)

          javaMap.put("Devices", javaSet)
          //
          val response = coreApi.SendPushMessageByDevice(javaMap)
          Logger.info("Puship response: " + response)

      }
  }
}
