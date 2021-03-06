import helper.TestValueStore
import play.api.Logger
import play.api.libs.json.{ JsObject, Json }
import play.api.test.FakeRequest
import play.api.test.Helpers._
import testHelper.{ Helper, StartedApp }
import testHelper.Helper._
import testHelper.TestConfig._

/**
 * User: Björn Reimer
 * Date: 03.12.14
 * Time: 18:26
 */
class VerificationSpec extends StartedApp {

  sequential

  "Verification" should {

    val phoneNumber = "+4912345"
    val phoneNumber2 = "+49123456"
    val phoneNumber3 = "+491234567"
    val mail = "devnull@cameo.io"
    val mail2 = "devnull2@cameo.io"
    val mail3 = "devnull3@cameo.io"
    val mail4 = "devnull4@cameo.io"

    var testUser: TestUser = null

    step(TestValueStore.start())

    "create account with email" in {
      TestUser.create(None, Some(mail))
      1 === 1
    }

    "should have received verification email with link" in {
      Helper.waitFor(TestValueStore.getValues("mail").length == 1)
      val mail = TestValueStore.getValues("mail")(0)
      (mail \ "body").as[String] must contain("https://")
    }

    step(TestValueStore.stop())

    step(TestValueStore.start())

    "create account with phonenumber" in {
      TestUser.create(Some(phoneNumber), None)
      1 === 1
    }

    "should have received verification sms with link" in {
      Helper.waitFor(TestValueStore.getValues("sms").length == 1)
      val sms = TestValueStore.getValues("sms")(0)
      (sms \ "body").as[String] must contain("https://")
    }

    step(TestValueStore.stop())

    step(TestValueStore.start())

    "create account with phonenumber and email" in {
      TestUser.create(Some(phoneNumber), Some(mail))
      1 === 1
    }

    "should have received verification sms and email with link" in {
      Helper.waitFor(TestValueStore.getValues("sms").length == 1 && TestValueStore.getValues("mail").length == 1)
      val mail = TestValueStore.getValues("sms")(0)
      val sms = TestValueStore.getValues("sms")(0)
      (sms \ "body").as[String] must contain("https://")
      (mail \ "body").as[String] must contain("https://")
    }

    step(TestValueStore.stop())

    "create testUser without mail or phoneNumber" in {
      testUser = TestUser.create(None, None)
      1 === 1
    }

    step(TestValueStore.start())

    "update phonenumber of account" in {
      val path = basePath + "/account"
      val json = Json.obj("phoneNumber" -> phoneNumber)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "should have received verification sms with link" in {
      Helper.waitFor(TestValueStore.getValues("sms").length == 1)
      val sms = TestValueStore.getValues("sms")(0)
      (sms \ "body").as[String] must contain("https://")
    }

    step(TestValueStore.stop())

    step(TestValueStore.start())

    "update email of account" in {
      val path = basePath + "/account"
      val json = Json.obj("email" -> mail)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "should have received verification email with link" in {
      Helper.waitFor(TestValueStore.getValues("mail").length == 1)
      val email = TestValueStore.getValues("mail")(0)
      (email \ "body").as[String] must contain("https://")
    }

    step(TestValueStore.stop())

    step(TestValueStore.start())

    "update email and phonenumber of account" in {
      val path = basePath + "/account"
      val json = Json.obj("email" -> mail, "phoneNumber" -> phoneNumber)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    var verifyEmail = ""
    var verifyPhoneNumber = ""

    "should have received verification email and sms with link" in {
      Helper.waitFor(TestValueStore.getValues("sms").length == 1 && TestValueStore.getValues("mail").length == 1)
      val mail = TestValueStore.getValues("mail")(0)
      val sms = TestValueStore.getValues("sms")(0)
      (sms \ "body").as[String] must contain("https://")
      (mail \ "body").as[String] must contain("https://")
      verifyEmail = getCodeFromMessage((mail \ "body").as[String])
      verifyPhoneNumber = getCodeFromMessage((sms \ "body").as[String])
      1 === 1
    }
    step(TestValueStore.stop())

    "get account, mail and phonenumber should not be verified" in {
      val path = basePath + "/account"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "phoneNumber" \ "isVerified").asOpt[Boolean] must beSome(false)
      (data \ "email" \ "isVerified").asOpt[Boolean] must beSome(false)
    }

    "use link to verify email" in {
      val path = "/vr/" + verifyEmail

      val req = FakeRequest(GET, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      contentAsString(res) must contain("success")
    }

    "get account, mail should now be marked as verified" in {
      val path = basePath + "/account"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "email" \ "isVerified").asOpt[Boolean] must beSome(true)
      (data \ "phoneNumber" \ "isVerified").asOpt[Boolean] must beSome(false)
    }

    "use link to verify phoneNumber" in {
      val path = "/vr/" + verifyPhoneNumber

      val req = FakeRequest(GET, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      contentAsString(res) must contain("success")
    }

    "get account, mail should now be marked as verified" in {
      val path = basePath + "/account"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "email" \ "isVerified").asOpt[Boolean] must beSome(true)
      (data \ "phoneNumber" \ "isVerified").asOpt[Boolean] must beSome(true)
    }

    "return error when verification has already been used" in {
      val path = "/vr/" + verifyPhoneNumber

      val req = FakeRequest(GET, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      contentAsString(res) must contain("expired")
    }

    step(TestValueStore.start())
    "update email and phoneNumber of account again" in {
      val path = basePath + "/account"
      val json = Json.obj("email" -> mail2, "phoneNumber" -> phoneNumber2)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    var verifyEmail2 = ""
    var verifyEmail3 = ""

    "should have received verification email with link" in {
      Helper.waitFor(TestValueStore.getValues("mail").length == 1)
      val mail = TestValueStore.getValues("mail")(0)
      (mail \ "body").as[String] must contain("https://")
      verifyEmail2 = getCodeFromMessage((mail \ "body").as[String])

      1 === 1
    }
    step(TestValueStore.stop())

    "the new values should not be verified" in {
      val path = basePath + "/account"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "email" \ "isVerified").asOpt[Boolean] must beSome(false)
      (data \ "email" \ "value").asOpt[String] must beSome(mail2)
      (data \ "phoneNumber" \ "isVerified").asOpt[Boolean] must beSome(false)
      (data \ "phoneNumber" \ "value").asOpt[String] must beSome(phoneNumber2)
    }

    step(TestValueStore.start())
    "update email of account yet again" in {
      val path = basePath + "/account"
      val json = Json.obj("email" -> mail3)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "should have received verification email with link" in {
      Helper.waitFor(TestValueStore.getValues("mail").length == 1)
      val mail = TestValueStore.getValues("mail")(0)
      (mail \ "body").as[String] must contain("https://")
      verifyEmail3 = getCodeFromMessage((mail \ "body").as[String])
      1 === 1
    }
    step(TestValueStore.stop())

    "the first verification link should not work anymore" in {
      val path = "/vr/" + verifyEmail2

      val req = FakeRequest(GET, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      contentAsString(res) must contain("error")
    }

    "the second verification link should work" in {
      val path = "/vr/" + verifyEmail3

      val req = FakeRequest(GET, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      contentAsString(res) must contain("success")
    }

    "refuse invalid verification id" in {
      val path = "/vr/" + verifyEmail3

      val req = FakeRequest(GET, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      contentAsString(res) must contain("expired")
    }

    step(TestValueStore.start())
    "update email and phoneNumber of account" in {
      val path = basePath + "/account"
      val json = Json.obj("email" -> mail4, "phoneNumber" -> phoneNumber3)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    var verifyEmail4 = ""
    var verifyPhoneNumber2 = ""
    "should have received verification email with link" in {
      Helper.waitFor(TestValueStore.getValues("sms").length == 1 && TestValueStore.getValues("mail").length == 1)
      val mail = TestValueStore.getValues("mail")(0)
      val sms = TestValueStore.getValues("sms")(0)
      (sms \ "body").as[String] must contain("https://")
      (mail \ "body").as[String] must contain("https://")
      verifyEmail4 = getCodeFromMessage((mail \ "body").as[String])
      verifyPhoneNumber2 = getCodeFromMessage((sms \ "body").as[String])
      1 === 1
    }
    step(TestValueStore.stop())

    step(TestValueStore.start())
    "start verification of phonenumber and email manually" in {
      val path = basePath + "/verify"
      val json = Json.obj("verifyEmail" -> true, "verifyPhoneNumber" -> true)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    var verifyEmailCode = ""
    var verifyPhoneNumberCode = ""

    "should have received verification email and sms with link" in {
      Helper.waitFor(TestValueStore.getValues("sms").length == 1 && TestValueStore.getValues("mail").length == 1)
      val mail = TestValueStore.getValues("mail")(0)
      val sms = TestValueStore.getValues("sms")(0)
      (sms \ "body").as[String] must contain("https://")
      (mail \ "body").as[String] must contain("https://")
      verifyEmailCode = (mail \ "body").as[String].split("\"")(1)
      verifyPhoneNumberCode = (sms \ "body").as[String].split("\"")(1)
      1 === 1
    }
    step(TestValueStore.stop())

    "refuse to verify with wrong account" in {
      val path = basePath + "/verify/" + verifyEmailCode

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
    }

    "verify email with code" in {
      val path = basePath + "/verify/" + verifyEmailCode

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "refuse to verify again" in {
      val path = basePath + "/verify/" + verifyEmailCode

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
    }

    "verify phonenumber with code" in {
      val path = basePath + "/verify/" + verifyPhoneNumberCode

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "email and phonenumber should now be verified" in {
      val path = basePath + "/account"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "email" \ "isVerified").asOpt[Boolean] must beSome(true)
      (data \ "email" \ "value").asOpt[String] must beSome(mail4)
      (data \ "phoneNumber" \ "isVerified").asOpt[Boolean] must beSome(true)
      (data \ "phoneNumber" \ "value").asOpt[String] must beSome(phoneNumber3)
    }

    "the old verification link should not work any more" in {
      val path = "/vr/" + verifyEmail4

      val req = FakeRequest(GET, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      contentAsString(res) must contain("expired")
    }


  }
}