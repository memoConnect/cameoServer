import helper.TestValueStore
import play.api.Logger
import play.api.libs.json.{ JsObject, Json }
import testHelper.{Stuff, StartedApp}
import testHelper.TestConfig._
import testHelper.Stuff._
import play.api.libs.json.{ JsArray, Json, JsObject }
import play.api.test.{ FakeRequest, FakeApplication }
import play.api.test.Helpers._

/**
 * User: Björn Reimer
 * Date: 3/3/14
 * Time: 4:48 PM
 */
class PushNotificationControllerSpec extends StartedApp {

  sequential

  "PushNotificationController" should {

    val deviceId1 = "moepDevice1"
    val deviceId2 = "moepDevice2"
    val platform2 = "meopPlatform2"
    val deviceId3 = "moepDevice3"
    val platform3 = "meopPlatform3"

    val languageValidEn = "en"
    val languageValidEn2 = "en"
    val languageValidDe = "de"
    val languageValidFr = "fr"
    val languageInvalid = "moep"

    "register ios device" in {
      val path = basePath + "/pushDevice"

      val json = Json.obj("deviceToken" -> deviceId1, "platform" -> "ios", "language" -> languageValidDe)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != 232) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(232)
    }

    "register android device" in {
      val path = basePath + "/pushDevice"

      val json = Json.obj("deviceToken" -> deviceId1, "platform" -> "and", "language" -> languageValidDe)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != 232) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(232)
    }

    "register windows device" in {
      val path = basePath + "/pushDevice"

      val json = Json.obj("deviceToken" -> deviceId1, "platform" -> "win", "language" -> languageValidDe)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != 232) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(232)
    }

    "refuse invalid platform" in {
      val path = basePath + "/pushDevice"

      val json = Json.obj("deviceToken" -> deviceId1, "platform" -> "moep", "language" -> languageValidDe)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
    }

    "refulse invalid lang code" in {
      val path = basePath + "/pushDevice"

      val json = Json.obj("deviceId" -> deviceId1, "platform" -> "and", "language" -> languageInvalid)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
    }


    "delete ios device" in {
      val path = basePath + "/pushDevice/ios/" + deviceId1

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      if (status(res) != 232) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(232)
    }

    "delete android device" in {
      val path = basePath + "/pushDevice/and/" + deviceId1

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      if (status(res) != 232) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(232)
    }

    "delete windows device" in {
      val path = basePath + "/pushDevice/win/" + deviceId1

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      if (status(res) != 232) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(232)
    }

    "refuse to delete invalid platform" in {
      val path = basePath + "/pushDevice/moep/" + deviceId1

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
    }


    step(TestValueStore.start())

    "create conversation and add message" in {
      val path = basePath + "/conversation"

      val json = Json.obj("recipients" -> Seq(identityExisting), "messages" -> Seq(Json.obj("plain" -> Json.obj("text" -> "moep"))))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(internalContactToken))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "user should receive push notifications for the message" in {
      Stuff.waitFor(TestValueStore.getValues("push").length == 1 )

      val pushMessages = TestValueStore.getValues("push")

      pushMessages.find(js => (js \ "sendToIdentity").as[String].equals(identityExisting)) must beSome
    }

    step(TestValueStore.stop())

    step(TestValueStore.start())

    "send FriendRequest" in {
      val path = basePath + "/friendRequest"

      val json = Json.obj("identityId" -> identityExisting, "message" -> "friendMopeMessage")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting3)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "user should receive three push notifications for the friend request in the right languages" in {
      Stuff.waitFor(TestValueStore.getValues("push").length == 1 )

      val pushMessages = TestValueStore.getValues("push")

      pushMessages.find(js => (js \ "sendToIdentity").as[String].equals(identityExisting)) must beSome
    }

    step(TestValueStore.stop())

  }
}