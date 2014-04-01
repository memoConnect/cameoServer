package cockpit

import play.api.libs.json.{Json, JsObject}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import testHelper.TestConfig._
import testHelper.Stuff._
import testHelper.StartedApp
import helper.TestValueStore
import scala.concurrent.Await
import scala.concurrent.duration._
import play.api.libs.json.JsObject

class CockpitControllerSpec extends StartedApp {
  sequential

  val allRoutes = app.routes.get.documentation.map { r => (r._1, r._2) }
  val cockpitRoutes = allRoutes.filter(route => route._2.startsWith("/api/cockpit") && !route._2.contains("twoFactorAuth"))
  def addIds(str: String) = str.replaceAll("\\$[^\\>]+", randomLengthString(16)).replace(">", "")
  val cockpitRoutesWithIds = cockpitRoutes.map { r =>
    (r._1, addIds(r._2))
  }
  "CockpitEditController" should {

    var twoFactorToken = ""

    step(TestValueStore.start())

    "get two factor auth token" in {

      val path1 = basePath + "/twoFactorAuth"
      val req1 = FakeRequest(GET, path1).withHeaders(tokenHeader(tokenExisting))
      val res1 = route(req1).get

      status(res1) must equalTo(OK)

      Await.result(res1, Duration.create(1, MINUTES) )

      Thread.sleep(300)

      val sms = TestValueStore.getValues("sms").filter(js => (js \ "from").asOpt[String].getOrElse("").contains("Two Factor"))

      val smsKey = (sms(0) \ "body").as[String]

      val path2 = basePath + "/twoFactorAuth/confirm"

      val json = Json.obj("key" -> smsKey)

      val req2 = FakeRequest(POST, path2).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res2 = route(req2).get

      status(res2) must equalTo(OK)

      val data = (contentAsJson(res2) \ "data").as[JsObject]

      (data \ "token").asOpt[String] must beSome
      twoFactorToken = (data \ "token").as[String]
      (data \ "created").asOpt[Long] must beSome
    }

    step(TestValueStore.stop())


    cockpitRoutesWithIds.seq.map { r =>

      r._1 + " " + r._2 + "\t: should work for users on access list" in {
        val method = r._1
        val path = r._2

        val json = Json.obj()

        val req = FakeRequest(method, path).withHeaders(tokenHeader(tokenExisting), twoFactorTokenHeader(twoFactorToken)).withJsonBody(json)
        val res = route(req).get

        status(res) must not equalTo(UNAUTHORIZED)
      }
    }

    var twoFactorToken2 = ""

    step(TestValueStore.start())

    "get two factor auth token" in {

      val path1 = basePath + "/twoFactorAuth"
      val req1 = FakeRequest(GET, path1).withHeaders(tokenHeader(tokenExisting2))
      val res1 = route(req1).get

      status(res1) must equalTo(OK)

      Await.result(res1, Duration.create(1, MINUTES) )

      Thread.sleep(300)

      val sms = TestValueStore.getValues("sms").filter(js => (js \ "from").asOpt[String].getOrElse("").contains("Two Factor"))

      val smsKey = (sms(0) \ "body").as[String]

      val path2 = basePath + "/twoFactorAuth/confirm"

      val json = Json.obj("key" -> smsKey)

      val req2 = FakeRequest(POST, path2).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res2 = route(req2).get

      status(res2) must equalTo(OK)

      val data = (contentAsJson(res2) \ "data").as[JsObject]

      (data \ "token").asOpt[String] must beSome
      twoFactorToken2 = (data \ "token").as[String]
      (data \ "created").asOpt[Long] must beSome

    }

    step(TestValueStore.stop())

    cockpitRoutesWithIds.seq.map { r =>

      r._1 + " " + r._2 + "\t: should not work for users not on access list" in {
        val method = r._1
        val path = r._2

        val json = Json.obj()

        val req = FakeRequest(method, path).withHeaders(tokenHeader(tokenExisting2), twoFactorTokenHeader(twoFactorToken2)).withJsonBody(json)
        val res = route(req).get

        status(res) must equalTo(UNAUTHORIZED)
      }
    }

    "foo" in {
      1 === 1
    }

  }
}

