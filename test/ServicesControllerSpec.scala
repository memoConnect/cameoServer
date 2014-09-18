/**
 * User: Björn Reimer
 * Date: 2/4/14
 * Time: 10:44 AM
 */

import helper.Utils
import org.specs2.mutable._
import play.api.{Play, Logger}
import play.api.libs.json.{ JsObject, Json }
import play.api.test.{ FakeApplication, WithApplication, FakeRequest }
import play.api.test.Helpers._
import testHelper.{ TestConfig, StartedApp }
import testHelper.TestConfig._
import play.api.Play.current

class ServicesControllerSpec extends StartedApp {

  "ServicesController" should {

    val acceptedVersions = Seq(
      ("1", "1"),
      ("0.2.6", "0.3.0"),
      ("1" , "2"),
      ("3.1", "3.1"),
      ("3.1", "3.2"),
      ("3.1", "4.1"),
      ("3.1", "5.5"),
      ("2.1.3", "2.1.3"),
      ("2.1.3", "2.1.3.12"),
      ("3.1.23.2", "3.1.23.3")
    )

    val rejectedVersions = Seq(
      ("2", "1"),
      ("2.1", "1"),
      ("2.1", "2"),
      ("2.3", "2.1"),
      ("3.15.1", "3.15"),
      ("4.2.12.3", "4.2.12.2")
    )

    val invalidVersions = Seq(
      "asdf",
      "",
      ".",
      "asdf.asdf",
      ".3.4",
      "2.3.asdf"
    )

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

    "Return browser info without version" in {
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

    "Accept supported client version" in {
      val path = basePath + "/services/getBrowserInfo"
      val currentVersion = Play.configuration.getString("client.version.min")

      val json = Json.obj("version" -> currentVersion)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "languageCode").asOpt[String] must beSome
      (data \ "versionIsSupported").asOpt[Boolean] must beSome(true)
    }

//    "Reject unsupported client version" in {
//      val path = basePath + "/services/getBrowserInfo"
//
//      val json = Json.obj("version" -> "0.1")
//
//      val req = FakeRequest(POST, path).withJsonBody(json)
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//
//      val data = (contentAsJson(res) \ "data").as[JsObject]
//      (data \ "languageCode").asOpt[String] must beSome
//      (data \ "versionIsSupported").asOpt[Boolean] must beSome(false)
//    }
//
//    invalidVersions.map {
//      version =>
//        "detect invalid version string: " + version  in {
//          val path = basePath + "/services/getBrowserInfo"
//
//          val json = Json.obj("version" -> version)
//
//          val req = FakeRequest(POST, path).withJsonBody(json)
//          val res = route(req).get
//
//          if (status(res) != BAD_REQUEST) {
//            Logger.error("Response: " + contentAsString(res))
//          }
//          status(res) must equalTo(BAD_REQUEST)
//        }
//    }

//    acceptedVersions.map{
//      case(supported, client) =>
//        "accept version combination: " + supported + " => " + client in {
//          Utils.compareVersions(supported, client) must beTrue
//        }
//    }

//
//    rejectedVersions.map{
//      case(supported, client) =>
//        "reject version combination: " + supported + " => " + client in {
//          Utils.compareVersions(supported, client) must beFalse
//        }
//    }

    "1is1" in {
      1 === 1
    }
  }

}
