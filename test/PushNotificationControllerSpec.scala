import play.api.Logger
import play.api.libs.json.{JsObject, Json}
import testHelper.StartedApp
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

    "add device id to account" in {
      val path = basePath + "/deviceId"

      val json = Json.obj("deviceId" -> deviceId1)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "account should contain the device id" in {

      val path = basePath + "/account"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "deviceIds").asOpt[Seq[String]] must beSome(contain(exactly(deviceId1)))
    }

    "add another device Id" in {
      val path = basePath + "/deviceId"

      val json = Json.obj("deviceId" -> deviceId2)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "account should contain the second device id"  in {
      val path = basePath + "/account"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "deviceIds").asOpt[Seq[String]] must beSome(contain(exactly(deviceId1, deviceId2)))
    }

    "add the same device id again" in {
      val path = basePath + "/deviceId"

      val json = Json.obj("deviceId" -> deviceId1)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "account should still contain the same device ids"  in {
      val path = basePath + "/account"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "deviceIds").asOpt[Seq[String]] must beSome(contain(exactly(deviceId1, deviceId2)))
    }

    "add device id to another account" in {
      val path = basePath + "/deviceId"

      val json = Json.obj("deviceId" -> deviceId1)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "second account should now contain that device id" in {
      val path = basePath + "/account"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "deviceIds").asOpt[Seq[String]] must beSome(contain(exactly(deviceId1)))
    }

    "first account should not contain this device id any more" in {
      val path = basePath + "/account"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "deviceIds").asOpt[Seq[String]] must beSome(contain(exactly(deviceId2)))
    }

    "delete device id" in {
      val path = basePath + "/deviceId/" + deviceId1

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "account should not contain this device id any more" in {
      val path = basePath + "/account"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "deviceIds").asOpt[Seq[String]] must beSome
      (data \ "deviceIds").as[Seq[String]] must have size 0
    }
  }
}
