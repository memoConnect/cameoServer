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
      ("2.1.3", "2.2.0"),
      ("0.2.6", "0.3.0"),
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

          status(res) aka "http result code bad request" must equalTo(232)

          val resStatus = (contentAsJson(res) \ "res").as[String]
          resStatus must beEqualTo("KO")

          val errorMsg = (contentAsJson(res) \ "error").asOpt[String]
          errorMsg must beSome
      }
    }

    "Check valid emails " in {
      val path = basePath + "/services/checkEmailAddress"

      TestConfig.validEmails.map {
        email =>
          val json = Json.obj("emailAddress" -> email)

          val req = FakeRequest(POST, path).withJsonBody(json)
          val res = route(req).get

          if (status(res) != OK) {
            Logger.error("Response: " + contentAsString(res))
          }
          status(res) must equalTo(OK)

          val resStatus = (contentAsJson(res) \ "res").as[String]
          resStatus must beEqualTo("OK")

          val data = (contentAsJson(res) \ "data").as[JsObject]
          val cleaned = (data \ "email").as[String]
          cleaned must beEqualTo(email)
      }
    }

    "Check invalid emails " in {
      val path = basePath + "/services/checkEmailAddress"
      TestConfig.invalidEmails.map {
        email =>
          val json = Json.obj("emailAddress" -> email)

          val req = FakeRequest(POST, path).withJsonBody(json)
          val res = route(req).get

          status(res) aka "http result code bad request" must equalTo(232)

          val resStatus = (contentAsJson(res) \ "res").as[String]
          resStatus must beEqualTo("KO")

          val errorMsg = (contentAsJson(res) \ "error").asOpt[String]
          errorMsg must beSome
      }
    }

    "Check valid phoneNumbers in mixed field" in {
      val path = basePath + "/services/checkMixed"

      TestConfig.validPhoneNumbers.map {
        case (unclean, clean) =>
          val json = Json.obj("mixed" -> unclean)

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

    "Check invalid phoneNumbers in mixed field" in {
      val path = basePath + "/services/checkMixed"
      TestConfig.invalidPhoneNumbers.map {
        phoneNumber =>
          val json = Json.obj("mixed" -> phoneNumber)

          val req = FakeRequest(POST, path).withJsonBody(json)
          val res = route(req).get

          status(res) aka "http result code bad request" must equalTo(232)

          val resStatus = (contentAsJson(res) \ "res").as[String]
          resStatus must beEqualTo("KO")

          val errorMsg = (contentAsJson(res) \ "error").asOpt[String]
          errorMsg must beSome
      }
    }

    "Check valid emails in mixed field" in {
      val path = basePath + "/services/checkMixed"

      TestConfig.validEmails.map {
        email =>
          val json = Json.obj("mixed" -> email)

          val req = FakeRequest(POST, path).withJsonBody(json)
          val res = route(req).get

          if (status(res) != OK) {
            Logger.error("Response: " + contentAsString(res))
          }
          status(res) must equalTo(OK)

          val resStatus = (contentAsJson(res) \ "res").as[String]
          resStatus must beEqualTo("OK")

          val data = (contentAsJson(res) \ "data").as[JsObject]
          val cleaned = (data \ "email").as[String]
          cleaned must beEqualTo(email)
      }
    }

    "Check invalid phoneNumbers in mixed field" in {
      val path = basePath + "/services/checkMixed"
      TestConfig.invalidEmails.map {
        email =>
          val json = Json.obj("mixed" -> email)

          val req = FakeRequest(POST, path).withJsonBody(json)
          val res = route(req).get

          status(res) aka "http result code bad request" must equalTo(232)

          val resStatus = (contentAsJson(res) \ "res").as[String]
          resStatus must beEqualTo("KO")

          val errorMsg = (contentAsJson(res) \ "error").asOpt[String]
          errorMsg must beSome
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

    "redirect to Apple App Store" in {
      val path = "/as"

      val req = FakeRequest(GET, path).withHeaders(("User-Agent", "Mozilla/5.0 (iPhone; U; CPU iPhone OS 3_0 like Mac OS X; en-us) AppleWebKit/528.18 (KHTML, like Gecko) Version/4.0 Mobile/7A341 Safari/528.16"))
      val res = route(req).get

      if (status(res) != TEMPORARY_REDIRECT) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(TEMPORARY_REDIRECT)
    }
  }

}
