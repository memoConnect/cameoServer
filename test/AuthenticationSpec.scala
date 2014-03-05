
import play.api.libs.json.JsArray
import play.api.test._
import play.api.libs.json.{JsArray, Json, JsObject}
import play.api.test.FakeApplication
import play.api.test.Helpers._
import play.core.Router
import scala.Some
import testHelper.MockupFactory._
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import play.api.{Play, Logger}
import testHelper.MockupFactory
import org.specs2.mutable._
import testHelper.Config._

/**
 * User: Björn Reimer
 * Date: 3/3/14
 * Time: 4:48 PM
 */
class AuthenticationSpec extends Specification {

  sequential

  val invalidToken = "invalid"


  "Authentication" should {

    step(play.api.Play.start(app))

    val allRoutes = app.routes.get.documentation.map{r => (r._1, r._2)}

    val nonAuthRoutes: Seq[(String, String)] = Seq(
      (POST, "/api/v1/services/checkEmailAddress"),
      (POST, "/api/v1/services/checkPhoneNumber"),
      (POST, "/api/v1/identity/search"),
      (POST,"/api/v1/account"),
      (POST,"/api/v1/account/check"),
      (GET, "/api/v1/token"),
      (GET, "/api/v1"),
      (GET, "/api/v1/purl/AthaUuGR"),
      (GET, "/api/v1/identity/$id<[^/]+>"),
      (GET, "/api/v1/verify/$id<[^/]+>"),
      (GET, "/v/$id<[^/]+>"),
      (GET, "/p/$id<[^/]+>"),
      (GET, "/api/v1/purl/$id<[^/]+>")
    )


    // all routes not specified as nonAuth are assumed to be auth
    val authRoutes: Seq[(String, String)] = allRoutes.sorted.diff(nonAuthRoutes.sorted)

    // dont test utils and webapp
    val filteredAuthRoutes = authRoutes.filterNot(r =>
      r._2.startsWith("/app") ||
        r._2.equals("/") ||
        r._2.startsWith("/api/v1/util") ||
        r._1.equals("OPTIONS")
    )

    // add random ids to routes
    def addIds(str: String) = str.replaceAll("\\$[^\\>]+", randomLengthString(16)).replace(">","")
    val authRoutesWithIds = filteredAuthRoutes.map { r=>
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

      nonAuthRoutesWithIds.map { r=>

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

    step(play.api.Play.stop())
  }

}