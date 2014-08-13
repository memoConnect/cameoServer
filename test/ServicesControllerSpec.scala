/**
 * User: Björn Reimer
 * Date: 2/4/14
 * Time: 10:44 AM
 */

import org.specs2.mutable._
import play.api.Logger
import play.api.libs.json.{ JsObject, Json }
import play.api.test.{ FakeApplication, WithApplication, FakeRequest }
import play.api.test.Helpers._
import testHelper.{ TestConfig, StartedApp }
import testHelper.TestConfig._

class ServicesControllerSpec extends StartedApp {

  "ServicesController" should {

    "Check valid phoneNumbers " in {
      val path = basePath + "/services/checkPhoneNumber"

      TestConfig.validPhoneNumbers.map {
        case (unclean, clean) =>
          val json = Json.obj("phoneNumber" -> unclean)

          val req = FakeRequest(POST, path).withJsonBody(json)
          val res = route(req).get

          if (status(res) != OK) {
            Logger.error("Response: " + contentAsString(res))
          }
          status(res) must equalTo(OK)

          val resStatus = (contentAsJson(res) \ "res").as[String]
          resStatus must beEqualTo("OK")

          val data = (contentAsJson(res) \ "data").as[JsObject]
          val cleanedPhoneNumber = (data \ "phoneNumber").as[String]
          cleanedPhoneNumber must beEqualTo(clean)
      }
    }

    "Check invalid phoneNumbers " in {
      val path = basePath + "/services/checkPhoneNumber"
      TestConfig.invalidPhoneNumbers.map {
        phoneNumber =>
          val json = Json.obj("phoneNumber" -> phoneNumber)

          val req = FakeRequest(POST, path).withJsonBody(json)
          val res = route(req).get

          status(res) aka "http result code bad request" must equalTo(BAD_REQUEST)

          val resStatus = (contentAsJson(res) \ "res").as[String]
          resStatus must beEqualTo("KO")

          val errorMsg = (contentAsJson(res) \ "error").asOpt[String]
          errorMsg must beSome
      }
    }

    "Check valid email addresses " in {
      val path = basePath + "/services/checkEmailAddress"
      TestConfig.validEmails.map {
        emailAddress =>
          val json = Json.obj("emailAddress" -> emailAddress)
          val req = FakeRequest(POST, path).withJsonBody(json)
          val res = route(req).get

          status(res) aka "http result code OK" must equalTo(OK)
          val resStatus = (contentAsJson(res) \ "res").as[String]
          resStatus aka "result status OK" must beEqualTo("OK")
      }
    }

    "Check invalid email addresses " in {
      val path = basePath + "/services/checkEmailAddress"
      TestConfig.invalidEmails.map {
        emailAddress =>
          val json = Json.obj("emailAddress" -> emailAddress)
          val req = FakeRequest(POST, path).withJsonBody(json)
          val res = route(req).get

          status(res) aka "http result code bad request" must equalTo(BAD_REQUEST)
          val resStatus = (contentAsJson(res) \ "res").as[String]
          resStatus aka "result status KO" must beEqualTo("KO")
      }
    }

    "Return browser info" in {
      val path = basePath + "/services/getBrowserInfo"

      val req = FakeRequest(GET, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "languageCode").asOpt[String] must beSome
    }
  }

}
