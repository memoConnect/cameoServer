import helper.TestValueStore
import play.api.libs.json.{Json, JsObject}
import play.api.Logger
import play.api.test.FakeRequest
import play.api.test.Helpers._
import testHelper.TestConfig._
import testHelper.Stuff._
import testHelper.StartedApp

/**
 * User: Björn Reimer
 * Date: 3/13/14
 * Time: 6:12 PM
 */
class TwoFactorControllerSpec extends StartedApp {

  sequential

  "TwoFactorController" should {

    var smsKey = ""
    var twoFactorToken = ""

    step(TestValueStore.start())

    "Send a new SmsKey to User with valid token" in {
      val path = basePath + "/twoFactorAuth"
      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "check if smsKey was received" in {

      Thread.sleep(500)

      val sms = TestValueStore.getValues("sms").filter(js => (js \ "from").asOpt[String].getOrElse("").contains("Two Factor"))
      sms.size must beEqualTo(1)

      (sms(0) \ "body").asOpt[String] must beSome
      smsKey = (sms(0) \ "body").as[String]

      smsKey.length() must beEqualTo(8)
    }

    step(TestValueStore.stop())

    "refuse to return two factor token to other identity" in {
      val path = basePath + "/twoFactorAuth/confirm"

      val json = Json.obj("key" -> smsKey)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
    }

    "get two factor token with smsKey" in {
      val path = basePath + "/twoFactorAuth/confirm"

      val json = Json.obj("key" -> smsKey)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "token").asOpt[String] must beSome
      twoFactorToken = (data \ "token").as[String]
      (data \ "created").asOpt[String] must beSome
    }

    "refuse to return another two factor token with same smsKey" in {
      val path = basePath + "/twoFactorAuth/confirm"

      val json = Json.obj("key" -> smsKey)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
    }

    "refuse to return two factor token with invalid smsKey" in {
      val path = basePath + "/twoFactorAuth/confirm"

      val json = Json.obj("key" -> "moep")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
    }


  }
}

