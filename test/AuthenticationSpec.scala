
import helper.TestHelper
import play.api.libs.json.JsArray
import play.api.libs.json.JsObject
import play.api.test._
import play.api.libs.json.{ JsArray, Json, JsObject }
import play.api.test.FakeApplication
import play.api.test.Helpers._
import play.core.Router
import scala.concurrent.Await
import scala.Some
import testHelper.Stuff._
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import play.api.{ Play, Logger }
import testHelper.{ StartedApp, Stuff }
import org.specs2.mutable._
import testHelper.TestConfig._
import scala.concurrent.duration._

/**
 * User: Björn Reimer
 * Date: 3/3/14
 * Time: 4:48 PM
 */
class AuthenticationSpec extends StartedApp {

  sequential

  val invalidToken = "invalid"

  "Authentication" should {

    val allRoutes = app.routes.get.documentation.map { r => (r._1, r._2) }

    val nonAuthRoutes: Seq[(String, String)] = Seq(
      (POST, "/api/v1/services/checkEmailAddress"),
      (POST, "/api/v1/services/checkPhoneNumber"),
     // (POST, "/api/v1/identity/search"),
      (POST, "/api/v1/account"),
      (POST, "/api/v1/account/check"),
      (GET, "/api/v1/token"),
      (GET, "/api/v1"),
      (GET, "/api/v1/purl/AthaUuGR"),
      (GET, "/api/v1/identity/$id<[^/]+>"),
      (GET, "/api/v1/verify/$id<[^/]+>"),
      (GET, "/v/$id<[^/]+>"),
      (GET, "/p/$id<[^/]+>"),
      (GET, "/api/v1/purl/$id<[^/]+>")
    )

    val twoFactorAuthRoutes: Seq[(String, String)] =
      // all that contain /api/cockpit/
      allRoutes.filter(route => route._2.startsWith("/api/cockpit") && ! route._2.contains("twoFactorAuth"))

    // all routes not specified as nonAuth or twoFactorAuth are assumed to be auth
    val authRoutes: Seq[(String, String)] = allRoutes.sorted.diff(nonAuthRoutes.sorted).diff(twoFactorAuthRoutes.sorted)

    // dont test utils and webapp
    val filteredAuthRoutes = authRoutes.filterNot(r =>
      r._2.startsWith("/app") ||
        r._2.startsWith("/cockpit") ||
        r._2.equals("/") ||
        r._2.startsWith("/api/v1/util") ||
        r._1.equals("OPTIONS")
    )

    // add random ids to routes
    def addIds(str: String) = str.replaceAll("\\$[^\\>]+", randomLengthString(16)).replace(">", "")
    val authRoutesWithIds = filteredAuthRoutes.map { r =>
      (r._1, addIds(r._2))
    }
    val twoFactorAuthRoutesWithIds = twoFactorAuthRoutes.map { r =>
      (r._1, addIds(r._2))
    }
    val nonAuthRoutesWithIds = nonAuthRoutes.map { r =>
      (r._1, addIds(r._2))
    }

    authRoutesWithIds.seq.map { r =>

      "WITH AUTH - " + r._1 + " " + r._2 + "\t: should not work with invalid token" in {
        val method = r._1
        val path = r._2

        val json = Json.obj()

        val req = FakeRequest(method, path).withHeaders(tokenHeader(invalidToken)).withJsonBody(json)
        val res = route(req).get

        status(res) must equalTo(UNAUTHORIZED)
      }

      "WITH AUTH - " + r._1 + " " + r._2 + "\t: should not work without token" in {
        val method = r._1
        val path = r._2

        val json = Json.obj()

        val req = FakeRequest(method, path).withJsonBody(json)
        val res = route(req).get

        status(res) must equalTo(UNAUTHORIZED)
      }

      "WITH AUTH - " + r._1 + " " + r._2 + "\t: should work with valid token" in {
        val method = r._1
        val path = r._2

        val json = Json.obj()

        val req = FakeRequest(method, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
        val res = route(req).get

        status(res) must not equalTo(UNAUTHORIZED)
      }
    }

    var twoFactorToken = ""

    "get two factor auth token" in {

      TestHelper.clear()

      val path1 = basePath + "/twoFactorAuth"
      val req1 = FakeRequest(GET, path1).withHeaders(tokenHeader(tokenExisting))
      val res1 = route(req1).get

      status(res1) must equalTo(OK)

      Await.result(res1, Duration.create(1, MINUTES) )

      val sms = TestHelper.getValues("sms").filter(js => (js \ "from").asOpt[String].getOrElse("").contains("Two Factor"))

      val smsKey = (sms(0) \ "body").as[String]

      val path2 = basePath + "/twoFactorAuth/confirm"

      val json = Json.obj("key" -> smsKey)

      val req2 = FakeRequest(POST, path2).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res2 = route(req2).get

      status(res2) must equalTo(OK)

      val data = (contentAsJson(res2) \ "data").as[JsObject]

      (data \ "token").asOpt[String] must beSome
      twoFactorToken = (data \ "token").as[String]
      (data \ "created").asOpt[String] must beSome
    }

    twoFactorAuthRoutesWithIds.map { r =>

      "WITH TWO-FACTOR-AUTH - " + r._1 + " " + r._2 + "\t: should not work with invalid token" in {
        val method = r._1
        val path = r._2

        val json = Json.obj()

        val req = FakeRequest(method, path).withHeaders(tokenHeader(invalidToken)).withJsonBody(json)
        val res = route(req).get

        status(res) must equalTo(UNAUTHORIZED)

        (contentAsJson(res) \ "twoFactorRequired").asOpt[Boolean] must beNone
      }

      "WITH TWO-FACTOR-AUTH - " + r._1 + " " + r._2 + "\t: should not work without token" in {
        val method = r._1
        val path = r._2

        val json = Json.obj()

        val req = FakeRequest(method, path).withJsonBody(json)
        val res = route(req).get

        status(res) must equalTo(UNAUTHORIZED)

        (contentAsJson(res) \ "twoFactorRequired").asOpt[Boolean] must beNone
      }

      "WITH TWO-FACTOR-AUTH - " + r._1 + " " + r._2 + "\t: should not work with normal token only" in {
        val method = r._1
        val path = r._2

        val json = Json.obj()

        val req = FakeRequest(method, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
        val res = route(req).get

        status(res) must equalTo(UNAUTHORIZED)

        (contentAsJson(res) \ "twoFactorRequired").asOpt[Boolean] must beSome
      }

      "WITH TWO-FACTOR-AUTH - " + r._1 + " " + r._2 + "\t: should not work with normal token and invalid twoFactorToken" in {
        val method = r._1
        val path = r._2

        val json = Json.obj()

        val req = FakeRequest(method, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting), twoFactorTokenHeader("moep"))
        val res = route(req).get

        status(res) must equalTo(UNAUTHORIZED)

        (contentAsJson(res) \ "twoFactorRequired").asOpt[Boolean] must beSome
      }

      "WITH TWO-FACTOR-AUTH - " + r._1 + " " + r._2 + "\t: should not work with normal token and twoFactorToken of other user" in {
        val method = r._1
        val path = r._2

        val json = Json.obj()

        val req = FakeRequest(method, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting2), twoFactorTokenHeader(twoFactorToken))
        val res = route(req).get

        status(res) must equalTo(UNAUTHORIZED)

        (contentAsJson(res) \ "twoFactorRequired").asOpt[Boolean] must beSome
      }

      "WITH TWO-FACTOR-AUTH - " + r._1 + " " + r._2 + "\t: should work with normal token and twoFactorToken of right user" in {
        val method = r._1
        val path = r._2

        val json = Json.obj()

        val req = FakeRequest(method, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting), twoFactorTokenHeader(twoFactorToken))
        val res = route(req).get

        status(res) must not equalTo(UNAUTHORIZED)

        (contentAsJson(res) \ "twoFactorRequired").asOpt[Boolean] must beNone
      }
    }

    nonAuthRoutesWithIds.map { r =>

      "NO AUTH - " + r._1 + " " + r._2 + "\t: should work without token" in {
        val method = r._1
        val path = r._2

        val json = Json.obj()

        val req = FakeRequest(method, path).withJsonBody(json)
        val res = route(req).get

        status(res) must not equalTo(UNAUTHORIZED)
      }

      "NO AUTH - " + r._1 + " " + r._2 + "\t: should work with token" in {
        val method = r._1
        val path = r._2

        val json = Json.obj()

        val req = FakeRequest(method, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
        val res = route(req).get

        status(res) must not equalTo(UNAUTHORIZED)
      }
    }

    "1is1" in {
      1 === 1
    }
  }

}