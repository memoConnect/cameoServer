package test


import play.api.test._
import play.api.libs.json.JsObject
import play.api.test.Helpers._
import play.api.test.FakeApplication
import testHelper.MockupFactory._
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
//

import org.specs2.mutable._

class ControllerSpec extends Specification {

  val basePath = "/api/v1"
  val dbName = "cameo_test"

  sequential

  "Controllers" should {

    // Use the same FakeApplication for all tests, so the mongoConnection does not break
    lazy val app = FakeApplication(additionalConfiguration = Map("mongodb.db" -> dbName))
    step(play.api.Play.start(app))

    val login = randomString(8)
    val pass = randomString(8)
    var identityId = ""
    var token = ""

    "Create Account" in {
      val path = basePath + "/account"
      val json = createUser(login, pass)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      val identityOpt = (data \ "identities")(0).asOpt[String]

      if (identityOpt.isDefined) {
        identityId = identityOpt.get
      }

      identityOpt must beSome
      (data \ "id").asOpt[String] must beSome
    }

    "Refuse duplicate Account names" in {
      val path = basePath + "/account"
      val json = createUser(login, pass)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
    }

    "Return a token" in {
      val path = basePath + "/identity/" + identityId + "/token"

      val auth = new sun.misc.BASE64Encoder().encode((login + ":" + pass).getBytes)

      val req = FakeRequest(GET, path).withHeaders(("Authorization", auth))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      val tokenOpt = (data \ "token").asOpt[String]
      if (tokenOpt.isDefined) {
        token = tokenOpt.get
      }

      tokenOpt must beSome
    }

    "Automatically create an identity for a new account" in {
      val path = basePath + "/identity/" + identityId + "?token=" + token

      val req = FakeRequest(GET, path)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "userKey").asOpt[String] must beSome
    }

    "drop the test database" in {
      ReactiveMongoPlugin.db.drop()(ExecutionContext.Implicits.global)
      1 === 1
    }


    step(play.api.Play.stop())
  }
}