import helper.TestHelper
import play.api.libs.json.JsObject
import play.api.Logger
import play.api.test.FakeRequest
import play.api.test.Helpers._
import testHelper.TestConfig._
import testHelper.MockupFactory._
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

    "Send a new SmsKey to User with valid token" in {
      val path = basePath + "/twoFactorAuth"
      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "check if key was received" in {

      Logger.debug("sms" + TestHelper.getValues("sms"))
      val sms = TestHelper.getValues("sms").filter(js => (js \ "from").asOpt[String].getOrElse("").contains("Two Factor"))
      sms.size must beEqualTo(1)

      (sms(0) \ "body").asOpt[String] must beSome
      smsKey = (sms(0) \ "body").as[String]

      smsKey.length() must beEqualTo(8)
    }

  }
}

