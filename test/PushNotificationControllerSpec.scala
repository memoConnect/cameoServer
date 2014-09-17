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
    val platform1 = "meopPlatform1"
    val deviceId2 = "moepDevice2"
    val platform2 = "meopPlatform2"
    val deviceId3 = "moepDevice3"
    val platform3 = "meopPlatform3"

    val languageValidEn = "en-US"
    val languageValidEn2 = "en-GB"
    val languageValidDe = "de-DE"
    val languageValidFr = "fr-FR"
    val languageInvalid = "moep"

    "register push device" in {
      val path = basePath + "/pushDevice"

      val json = Json.obj("deviceToken" -> deviceId1, "platform" -> platform1, "language" -> languageValidDe)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

//    "account should contain the push device" in {
//      val path = basePath + "/account"
//
//      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//
//      val data = (contentAsJson(res) \ "data").as[JsObject]
//      (data \ "pushDevices").asOpt[Seq[JsObject]] must beSome
//      val devices = (data \ "pushDevices").as[Seq[JsObject]]
//      devices must have size 1
//      (devices(0) \ "deviceId").asOpt[String] must beSome(deviceId1)
//      (devices(0) \ "platform").asOpt[String] must beSome(platform1)
//      (devices(0) \ "language").asOpt[String] must beSome(languageValidDe)
//    }
//
//    "do not accept invalid lang code" in {
//      val path = basePath + "/pushDevice"
//
//      val json = Json.obj("deviceId" -> deviceId1, "platform" -> platform1, "language" -> languageInvalid)
//
//      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
//      val res = route(req).get
//
//      if (status(res) != BAD_REQUEST) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(BAD_REQUEST)
//    }
//
//    "add another push device" in {
//      val path = basePath + "/pushDevice"
//
//      val json = Json.obj("deviceId" -> deviceId2, "platform" -> platform2, "language" -> languageValidDe)
//
//      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//    }
//
//    "account should contain the second device id" in {
//      val path = basePath + "/account"
//
//      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//
//      val data = (contentAsJson(res) \ "data").as[JsObject]
//      (data \ "pushDevices").asOpt[Seq[JsObject]] must beSome
//      val devices = (data \ "pushDevices").as[Seq[JsObject]]
//      devices must have size 2
//      (devices(1) \ "deviceId").asOpt[String] must beSome(deviceId2)
//      (devices(1) \ "platform").asOpt[String] must beSome(platform2)
//      (devices(1) \ "language").asOpt[String] must beSome(languageValidDe)
//    }
//
//    "add the same device id again" in {
//      val path = basePath + "/pushDevice"
//
//      val json = Json.obj("deviceId" -> deviceId1, "platform" -> platform1, "language" -> languageValidEn)
//
//      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//    }
//
//    "account should still contain the same devices with updated information" in {
//      val path = basePath + "/account"
//
//      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//
//      val data = (contentAsJson(res) \ "data").as[JsObject]
//      (data \ "pushDevices").asOpt[Seq[JsObject]] must beSome
//      val devices = (data \ "pushDevices").as[Seq[JsObject]]
//      devices must have size 2
//      (devices(0) \ "deviceId").asOpt[String] must beSome(deviceId1)
//      (devices(0) \ "platform").asOpt[String] must beSome(platform1)
//      (devices(0) \ "language").asOpt[String] must beSome(languageValidEn)
//      (devices(1) \ "deviceId").asOpt[String] must beSome(deviceId2)
//      (devices(1) \ "platform").asOpt[String] must beSome(platform2)
//      (devices(1) \ "language").asOpt[String] must beSome(languageValidDe)
//    }
//
//    "add device to another account" in {
//      val path = basePath + "/pushDevice"
//
//      val json = Json.obj("deviceId" -> deviceId1, "platform" -> platform1, "language" -> languageValidEn)
//
//      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//    }
//
//    "second account should now contain that device id" in {
//      val path = basePath + "/account"
//
//      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//
//      val data = (contentAsJson(res) \ "data").as[JsObject]
//      (data \ "pushDevices").asOpt[Seq[JsObject]] must beSome
//      val devices = (data \ "pushDevices").as[Seq[JsObject]]
//      devices must have size 1
//      (devices(0) \ "deviceId").asOpt[String] must beSome(deviceId1)
//      (devices(0) \ "platform").asOpt[String] must beSome(platform1)
//      (devices(0) \ "language").asOpt[String] must beSome(languageValidEn)
//    }
//
//    "first account should not contain this device id any more" in {
//      val path = basePath + "/account"
//
//      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//
//      val data = (contentAsJson(res) \ "data").as[JsObject]
//      (data \ "pushDevices").asOpt[Seq[JsObject]] must beSome
//      val devices = (data \ "pushDevices").as[Seq[JsObject]]
//      devices must have size 1
//      (devices(0) \ "deviceId").asOpt[String] must beSome(deviceId2)
//      (devices(0) \ "platform").asOpt[String] must beSome(platform2)
//      (devices(0) \ "language").asOpt[String] must beSome(languageValidDe)
//    }
//
//    "delete device" in {
//      val path = basePath + "/pushDevice/" + deviceId1
//
//      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting2))
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//    }
//
//    "account should not contain this device id any more" in {
//      val path = basePath + "/account"
//
//      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//
//      val data = (contentAsJson(res) \ "data").as[JsObject]
//      (data \ "pushDevices").asOpt[Seq[JsObject]] must beSome
//      val devices = (data \ "pushDevices").as[Seq[JsObject]]
//      devices must have size 0
//    }
//
//    "add another device with another existing language" in {
//      val path = basePath + "/pushDevice"
//
//      val json = Json.obj("deviceId" -> deviceId1, "platform" -> platform1, "language" -> languageValidEn)
//
//      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//    }
//
//    "add another device with a language that does not exist" in {
//      val path = basePath + "/pushDevice"
//
//      val json = Json.obj("deviceId" -> deviceId3, "platform" -> platform3, "language" -> languageValidFr)
//
//      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//    }
//
//    step(TestValueStore.start())
//
//    "create conversation and add message" in {
//      val path = basePath + "/conversation"
//
//      val json = Json.obj("recipients" -> Seq(identityExisting), "messages" -> Seq(Json.obj("plain" -> Json.obj("text" -> "moep"))))
//
//      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(internalContactToken))
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//    }
//
//    "user should receive three push notifications for the message in the right languages" in {
//      Stuff.waitFor(TestValueStore.getValues("push").length == 3 )
//
//      val pushMessages = TestValueStore.getValues("push")
//
//      pushMessages.find(js => (js \ "deviceId").as[String].equals(deviceId1)) must beSome
//      pushMessages.find(js => (js \ "deviceId").as[String].equals(deviceId2)) must beSome
//      pushMessages.find(js => (js \ "deviceId").as[String].equals(deviceId3)) must beSome
//
//      val english = pushMessages.find(js => (js \ "deviceId").as[String].equals(deviceId1)).get
//      (english \ "message").asOpt[String] must beSome(contain(displayNameExisting) and contain(internalContactCameoId))
//      val englishText = (english \ "message").as[String]
//
//      val german = pushMessages.find(js => (js \ "deviceId").as[String].equals(deviceId2)).get
//      (german \ "message").asOpt[String] must beSome(contain(displayNameExisting) and contain(internalContactCameoId))
//      val germanText = (german \ "message").as[String]
//      germanText must not equalTo(englishText)
//
//      val french = pushMessages.find(js => (js \ "deviceId").as[String].equals(deviceId3)).get
//      (french \ "message").asOpt[String] must beSome(contain(displayNameExisting) and contain(internalContactCameoId))
//      val frenchText = (french \ "message").as[String]
//      frenchText must equalTo(englishText)
//    }
//
//    step(TestValueStore.stop())
//
//    step(TestValueStore.start())
//
//    "send FriendRequest" in {
//      val path = basePath + "/friendRequest"
//
//      val json = Json.obj("identityId" -> identityExisting, "message" -> "friendMopeMessage")
//
//      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting3)).withJsonBody(json)
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//    }
//
//    "user should receive three push notifications for the friend request in the right languages" in {
//      Stuff.waitFor(TestValueStore.getValues("push").length == 3 )
//
//      val pushMessages = TestValueStore.getValues("push")
//
//      pushMessages.find(js => (js \ "deviceId").as[String].equals(deviceId1)) must beSome
//      pushMessages.find(js => (js \ "deviceId").as[String].equals(deviceId2)) must beSome
//      pushMessages.find(js => (js \ "deviceId").as[String].equals(deviceId3)) must beSome
//
//      val english = pushMessages.find(js => (js \ "deviceId").as[String].equals(deviceId1)).get
//      (english \ "message").asOpt[String] must beSome(contain(displayNameExisting) and contain(cameoIdExisting3))
//      val englishText = (english \ "message").as[String]
//
//      val german = pushMessages.find(js => (js \ "deviceId").as[String].equals(deviceId2)).get
//      (german \ "message").asOpt[String] must beSome(contain(displayNameExisting) and contain(cameoIdExisting3))
//      val germanText = (german \ "message").as[String]
//      germanText must not equalTo(englishText)
//
//      val french = pushMessages.find(js => (js \ "deviceId").as[String].equals(deviceId3)).get
//      (french \ "message").asOpt[String] must beSome(contain(displayNameExisting) and contain(cameoIdExisting3))
//      val frenchText = (french \ "message").as[String]
//      frenchText must equalTo(englishText)
//    }
//
//    step(TestValueStore.stop())

  }
}