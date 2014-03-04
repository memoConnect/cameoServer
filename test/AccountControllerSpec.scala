
import play.api.libs.json.JsArray
import play.api.test._
import play.api.libs.json.{JsArray, Json, JsObject}
import play.api.test.FakeApplication
import play.api.test.Helpers._
import scala.Some
import testHelper.MockupFactory._
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import play.api.Logger
import testHelper.MockupFactory
import org.specs2.mutable._
import testHelper.Config._

/**
 * User: Björn Reimer
 * Date: 3/3/14
 * Time: 4:48 PM
 */
class AccountControllerSpec extends Specification {

  sequential

  val login = randomString(8)
  val login2 = randomString(8)
  val pass = randomString(8)
  val mail = "e@mail.de"
  val tel = "+491234567890"
  var identityId = ""
  var token = ""
  var regSec = ""
  var regSec2 = ""

  "AccountController" should {

    step(play.api.Play.start(app))

    "Refuse invalid Logins" in {

      val path = basePath + "/account/check"

      val logins = Seq("asdf", "asdfasdfasdfasdfasdfa", "..", ",asdf", "/asdf", "asdf#asdf", "asd£asdf", "<>", "\\")

      logins.map {
        l => {
          val json = Json.obj("loginName" -> l)

          val req = FakeRequest(POST, path).withJsonBody(json)
          val res = route(req).get

          status(res) aka ("UserName " + l) must equalTo(BAD_REQUEST)
        }
      }
    }

    "Reserve Login" in {
      val path = basePath + "/account/check"
      val json = Json.obj("loginName" -> login)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
      val data = (contentAsJson(res) \ "data").as[JsObject]

      val regSeqOpt = (data \ "reservationSecret").asOpt[String]

      if (regSeqOpt.isDefined) {
        regSec = regSeqOpt.get
      }

      regSeqOpt aka "returned registration secret" must beSome
    }

    "Reserve another Login" in {
      val path = basePath + "/account/check"
      val json = Json.obj("loginName" -> login2)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
      val data = (contentAsJson(res) \ "data").as[JsObject]

      val regSeqOpt = (data \ "reservationSecret").asOpt[String]

      if (regSeqOpt.isDefined) {
        regSec2 = regSeqOpt.get
      }

      regSeqOpt aka "returned registration secret" must beSome
    }

    "Refuse to reserve reserved loginName and return alternative" in {
      val path = basePath + "/account/check"
      val json = Json.obj("loginName" -> login)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(232)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "alternative").asOpt[String] must beSome(login + "_1")
    }

    "Refuse to claim reserved login without secret" in {
      val path = basePath + "/account"
      val json = createUser(login, pass, login)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
    }

    "Create Account" in {
      val path = basePath + "/account"
      val json = createUser(login, pass, login, Some(tel), Some(mail)) ++ Json.obj("reservationSecret" -> regSec)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      val identity = (data \ "identities")(0).as[JsObject]

      val identityOpt = (identity \ "id").asOpt[String]

      if (identityOpt.isDefined) {
        identityId = identityOpt.get
      }

      identityOpt must beSome
      (data \ "id").asOpt[String] must beSome
    }

    "Refuse to register with same secret" in {
      val path = basePath + "/account"
      val json = createUser(login, pass, login) ++ Json.obj("reservationSecret" -> regSec)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(232)
    }

    "Refuse duplicate CameoIds" in {
      val path = basePath + "/account"
      val json = createUser(login2, pass, login, Some(tel), Some(mail)) ++ Json.obj("reservationSecret" -> regSec2)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(232)

      val messages = (contentAsJson(res) \ "messages").asOpt[JsArray]

      messages must beSome
    }

    "Refuse to reserve existing loginName and return next alternative" in {
      val path = basePath + "/account/check"
      val json = Json.obj("loginName" -> login)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(232)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "alternative").asOpt[String] must beSome(login + "_1")
    }

    "Return a token" in {
      val path = basePath + "/token"

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
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "userKey").asOpt[String] must beSome
      (data \ "cameoId").asOpt[String] must beSome(login)
      (data \ "email" \ "value").asOpt[String] must beSome(mail)
      (data \ "phoneNumber" \ "value").asOpt[String] must beSome(tel)
    }

    step(play.api.Play.stop())
  }

}