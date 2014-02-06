/**
 * User: Björn Reimer
 * Date: 2/4/14
 * Time: 10:44 AM
 */

import org.specs2.mutable._
import play.api.libs.json.{JsObject, Json}
import play.api.test.{FakeApplication, WithApplication, FakeRequest}
import play.api.test.Helpers._


class ServicesSpec extends Specification {

  "Services" should {

    val basePath = "/api/v1"
    val dbName = "cameo_test"

    val additionalConfig = Map("mongodb.db" -> dbName ,
      "mongo.init.loadOnStart" -> "false",
      "embed.mongo.enabled" -> "false"
    )

    lazy val app = FakeApplication(additionalConfiguration = additionalConfig)
    step(play.api.Play.start(app))

    "Check valid phoneNumbers " in  {
      val path = basePath + "/services/checkPhoneNumber"
      Seq(
        List[String](" 0173-12  34dd5678", "+4917312345678"),
        List[String]("491234512345", "+491234512345"),
        List[String](" +17234512345         ", "+17234512345")
      ) map {
        phoneNumber =>
          val json = Json.obj("phoneNumber" -> phoneNumber.apply(0))

          val req = FakeRequest(POST, path).withJsonBody(json)
          val res = route(req).get

          status(res) must equalTo(OK)

          val resStatus = (contentAsJson(res) \ "res").as[String]
          resStatus must beEqualTo("OK")

          val data = (contentAsJson(res) \ "data").as[JsObject]
          val cleanedPhoneNumber = (data \ "phoneNumber").as[String]
          cleanedPhoneNumber must beEqualTo(phoneNumber.apply(1))
      }
    }

    "Check invalid phoneNumbers " in  {
      val path = basePath + "/services/checkPhoneNumber"
      Seq("abcd", "+4912345123451234512345", "", "+!\"§$%&/()=") map {
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

    "Check valid email addresses " in  {
      val path = basePath + "/services/checkEmailAddress"
      Seq("a-b.c_d@a-b.c_d.co", "123@345.fo", "123@3-4-5.fo") map {
        emailAddress =>
          val json = Json.obj("emailAddress" -> emailAddress)
          val req = FakeRequest(POST, path).withJsonBody(json)
          val res = route(req).get

          status(res) aka "http result code OK" must equalTo(OK)
          val resStatus = (contentAsJson(res) \ "res").as[String]
          resStatus aka "result status OK" must beEqualTo("OK")
      }
    }

    "Check invalid email addresses " in  {
      val path = basePath + "/services/checkEmailAddress"
      Seq("a@a.d", "a@a", "a@a aa.de", "a.de", "123@345.43").map {
        emailAddress =>
          val json = Json.obj("emailAddress" -> emailAddress)
          val req = FakeRequest(POST, path).withJsonBody(json)
          val res = route(req).get

          status(res) aka "http result code bad request" must equalTo(BAD_REQUEST)
          val resStatus = (contentAsJson(res) \ "res").as[String]
          resStatus aka "result status KO" must beEqualTo("KO")
      }
    }

    step(play.api.Play.stop())
  }

}
