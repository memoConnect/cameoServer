import helper.TestValueStore
import play.api.Logger
import play.api.libs.json.{ JsObject, Json }
import play.api.test.FakeRequest
import play.api.test.Helpers._
import testHelper.Helper._
import testHelper.TestConfig._
import testHelper.{ StartedApp, Helper }

/**
 * User: Björn Reimer
 * Date: 03.12.14
 * Time: 18:26
 */
class ResetPasswordSpec extends StartedApp {

  sequential

  "Password reset" should {

    val cameoId = "moepFooMoep"
    val phoneNumber = "+4913297234523"
    val mail = "a@trashmail.de"
    val newPassword = "moepHAHAmoep"
    val newPassword2 = "moepHAHAmoep2"

    var testUser: TestUser = null

    step(TestValueStore.start())
    "create account with email and phonenumber" in {
      testUser = TestUser.create(Some(phoneNumber), Some(mail))
      1 === 1
    }
    var verifyEmail = ""
    var verifyPhoneNumber = ""

    "receive verification email and sms" in {
      Helper.waitFor(TestValueStore.getValues("sms").length == 1 && TestValueStore.getValues("mail").length == 1)
      val mail = TestValueStore.getValues("mail")(0)
      val sms = TestValueStore.getValues("sms")(0)
      (sms \ "body").as[String] must contain("https://")
      (mail \ "body").as[String] must contain("https://")
      verifyEmail = (mail \ "body").as[String].split("https:").last.split("/").last
      verifyPhoneNumber = (sms \ "body").as[String].split("https:").last.split("/").last
      1 === 1
    }
    step(TestValueStore.stop())

    var regSec = ""
    "reserve cameoId" in {
      val path = basePath + "/account/check"
      val json = Json.obj("cameoId" -> cameoId)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "reservationSecret").asOpt[String] must beSome
      regSec = (data \ "reservationSecret").as[String]
      1 === 1
    }

    "add new identity to account" in {
      val path = basePath + "/identity"
      val json = Json.obj("displayName" -> "Foooooooooooooooo", "cameoId" -> cameoId, "reservationSecret" -> regSec)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "refuse to reset password for non existing login Name" in {
      val path = basePath + "/resetPassword"
      val json = Json.obj("identifier" -> "aaaaaaaaaaaaaaaaaaaaaaa")

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
    }

    "refuse to reset password for non-existing email" in {
      val path = basePath + "/resetPassword"
      val json = Json.obj("identifier" -> (mail + "x"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
      (contentAsJson(res) \ "errorCode").asOpt[String] must beSome("PASSWORD.RESET.EMAIL.NOT.FOUND")
    }

    "refuse to reset password for non-verified email" in {
      val path = basePath + "/resetPassword"
      val json = Json.obj("identifier" -> mail)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
      (contentAsJson(res) \ "errorCode").asOpt[String] must beSome("PASSWORD.RESET.EMAIL.NOT.FOUND")
    }

    "refuse to reset password for non-existing phoneNumber" in {
      val path = basePath + "/resetPassword"
      val json = Json.obj("identifier" -> (phoneNumber + "5"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
      (contentAsJson(res) \ "errorCode").asOpt[String] must beSome("PASSWORD.RESET.PHONENUMBER.NOT.FOUND")
    }

    "refuse to reset password for non-verified phoneNumber" in {
      val path = basePath + "/resetPassword"
      val json = Json.obj("identifier" -> phoneNumber)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
      (contentAsJson(res) \ "errorCode").asOpt[String] must beSome("PASSWORD.RESET.PHONENUMBER.NOT.FOUND")
    }

    "refuse password reset without verified mail or phoneNumber" in {
      val path = basePath + "/resetPassword"
      val json = Json.obj("identifier" -> testUser.login)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
      (contentAsJson(res) \ "errorCode").asOpt[String] must beSome("PASSWORD.RESET.NO.EMAIL.PHONENUMBER")
    }

    "verify email" in {
      val path = "/vr/" + verifyEmail

      val req = FakeRequest(GET, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      contentAsString(res) must contain("success")
    }

    step(TestValueStore.start())
    "request password reset via valid loginName" in {
      val path = basePath + "/resetPassword"
      val json = Json.obj("identifier" -> testUser.login)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "should have receive a confirmation email" in {
      Helper.waitFor(TestValueStore.getValues("mail").length == 1)
      val mail = TestValueStore.getValues("mail")(0)
      (mail \ "body").as[String] must contain("https://")
    }
    step(TestValueStore.stop())

    step(TestValueStore.start())
    "request password reset via cameoId" in {
      val path = basePath + "/resetPassword"
      val json = Json.obj("identifier" -> cameoId)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "should have receive a confirmation email" in {
      Helper.waitFor(TestValueStore.getValues("mail").length == 1)
      val mail = TestValueStore.getValues("mail")(0)
      (mail \ "body").as[String] must contain("https://")
    }
    step(TestValueStore.stop())

    step(TestValueStore.start())
    "request password reset via email" in {
      val path = basePath + "/resetPassword"
      val json = Json.obj("identifier" -> mail)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    var confirmCodeMail = ""
    "should have receive a confirmation email" in {
      Helper.waitFor(TestValueStore.getValues("mail").length == 1)
      val mail = TestValueStore.getValues("mail")(0)
      (mail \ "body").as[String] must contain("https://")
      confirmCodeMail = (mail \ "body").as[String].split("\"")(1)
      1 === 1
    }
    step(TestValueStore.stop())

    "refuse password reset with phoneNumber" in {
      val path = basePath + "/resetPassword"
      val json = Json.obj("identifier" -> phoneNumber)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
      (contentAsJson(res) \ "errorCode").asOpt[String] must beSome("PASSWORD.RESET.PHONENUMBER.NOT.FOUND")
    }

    "verify phoneNumber" in {
      val path = "/vr/" + verifyPhoneNumber

      val req = FakeRequest(GET, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      contentAsString(res) must contain("success")
    }

    step(TestValueStore.start())
    "request password reset via phoneNumber" in {
      val path = basePath + "/resetPassword"
      val json = Json.obj("identifier" -> phoneNumber)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    var confirmCodeMail2 = ""
    var confirmCodeSms = ""
    "should have receive a confirmation email and sms" in {
      Helper.waitFor(TestValueStore.getValues("mail").length == 1 && TestValueStore.getValues("sms").length == 1)
      val mail = TestValueStore.getValues("mail")(0)
      val sms = TestValueStore.getValues("sms")(0)
      (mail \ "body").as[String] must contain("https://")
      (sms \ "body").as[String] must contain("https://")
      confirmCodeMail2 = (mail \ "body").as[String].split("\"")(1)
      confirmCodeSms = (sms \ "body").as[String].split("\"")(1)
      1 === 1
    }
    step(TestValueStore.stop())

    "an old confirmation code should not work any more" in {
      val path = basePath + "/resetPassword/" + confirmCodeMail
      val json = Json.obj("newPassword" -> newPassword)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
      (contentAsJson(res) \ "errorCode").asOpt[String] must beSome("PASSWORD.RESET.EXPIRED")
    }

    "change password with confirmation code from mail" in {
      val path = basePath + "/resetPassword/" + confirmCodeMail2
      val json = Json.obj("newPassword" -> newPassword)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "the old password should not work any more" in {
      val path = basePath + "/token"

      val auth = "Basic " + new sun.misc.BASE64Encoder().encode((testUser.login + ":" + password).getBytes)

      val req = FakeRequest(GET, path).withHeaders(("Authorization", auth))
      val res = route(req).get

      if (status(res) != UNAUTHORIZED) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(UNAUTHORIZED)

    }

    "allow login with new password" in {
      val path = basePath + "/token"

      val auth = "Basic " + new sun.misc.BASE64Encoder().encode((testUser.login + ":" + newPassword).getBytes)

      val req = FakeRequest(GET, path).withHeaders(("Authorization", auth))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "the code from the sms should not work any more" in {
      val path = basePath + "/resetPassword/" + confirmCodeSms
      val json = Json.obj("newPassword" -> newPassword2)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
      (contentAsJson(res) \ "errorCode").asOpt[String] must beSome("PASSWORD.RESET.EXPIRED")
    }

    step(TestValueStore.start())
    "request password reset via phoneNumber again" in {
      val path = basePath + "/resetPassword"
      val json = Json.obj("identifier" -> phoneNumber)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    var confirmCodeMail3 = ""
    var confirmCodeSms2 = ""
    "should have receive a confirmation email and sms" in {
      Helper.waitFor(TestValueStore.getValues("mail").length == 1 && TestValueStore.getValues("sms").length == 1)
      val mail = TestValueStore.getValues("mail")(0)
      val sms = TestValueStore.getValues("sms")(0)
      (mail \ "body").as[String] must contain("https://")
      (sms \ "body").as[String] must contain("https://")
      confirmCodeMail3 = (mail \ "body").as[String].split("\"")(1)
      confirmCodeSms2 = (sms \ "body").as[String].split("\"")(1)
      1 === 1
    }
    step(TestValueStore.stop())


    "change password with confirmation code from sms" in {
      val path = basePath + "/resetPassword/" + confirmCodeSms2
      val json = Json.obj("newPassword" -> newPassword2)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "the old password should not work any more" in {
      val path = basePath + "/token"

      val auth = "Basic " + new sun.misc.BASE64Encoder().encode((testUser.login + ":" + newPassword).getBytes)

      val req = FakeRequest(GET, path).withHeaders(("Authorization", auth))
      val res = route(req).get

      if (status(res) != UNAUTHORIZED) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(UNAUTHORIZED)

    }

    "allow login with new password" in {
      val path = basePath + "/token"

      val auth = "Basic " + new sun.misc.BASE64Encoder().encode((testUser.login + ":" + newPassword2).getBytes)

      val req = FakeRequest(GET, path).withHeaders(("Authorization", auth))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "the code from the mail should not work any more" in {
      val path = basePath + "/resetPassword/" + confirmCodeMail3
      val json = Json.obj("newPassword" -> newPassword2)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
      (contentAsJson(res) \ "errorCode").asOpt[String] must beSome("PASSWORD.RESET.EXPIRED")
    }




  }
}