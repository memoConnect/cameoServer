
import play.api.libs.json.JsArray
import play.api.test._
import play.api.libs.json.{ JsArray, Json, JsObject }
import play.api.test.FakeApplication
import play.api.test.Helpers._
import scala.Some
import testHelper.Stuff._
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import play.api.Logger
import testHelper.{ TestConfig, StartedApp, Stuff }
import org.specs2.mutable._
import testHelper.TestConfig._

/**
 * User: Björn Reimer
 * Date: 3/3/14
 * Time: 4:48 PM
 */
class AccountControllerSpec extends StartedApp {

  sequential

  val login = randomString(8)
  val login2 = randomString(8)
  val pass = randomString(8)
  val mail = validEmails(0)
  val tel = validPhoneNumbers(0)._1
  val cleanedTel = validPhoneNumbers(0)._2
  var identityId = ""
  var token = ""
  var regSec = ""
  var regSec2 = ""

  "AccountController" should {

    "Refuse invalid Logins" in {
      val path = basePath + "/account/check"

      val logins = Seq("asdf", "asdfasdfasdfasdfasdfa", "..", ",asdf", "/asdf", "asdf#asdf", "asd£asdf", "<>", "\\", "asdf.asdf.asdf", "asd@df")

      logins.map {
        l =>
          {
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
      val json = createUser(login, pass)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
    }

    "Refuse to create account with invalid mails" in {
      val path = basePath + "/account"

      TestConfig.invalidEmails.map { invalid =>

        val json = createUser(login, pass, Some(tel), Some(invalid)) ++ Json.obj("reservationSecret" -> regSec)

        val req = FakeRequest(POST, path).withJsonBody(json)
        val res = route(req).get

        status(res) must equalTo(BAD_REQUEST)
      }
    }

    "Refuse to create account with invalid phoneNumbers" in {
      val path = basePath + "/account"

      TestConfig.invalidPhoneNumbers.map { invalid =>

        val json = createUser(login, pass,  Some(invalid), Some(mail)) ++ Json.obj("reservationSecret" -> regSec)

        val req = FakeRequest(POST, path).withJsonBody(json)
        val res = route(req).get

        status(res) must equalTo(BAD_REQUEST)
      }
    }

    "Refuse to register with wrong loginName for secret" in {
      val path = basePath + "/account"
      val json = createUser(login +"a", pass) ++ Json.obj("reservationSecret" -> regSec)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
    }

    "Create Account" in {
      val path = basePath + "/account"
      val json = createUser(login, pass, Some(tel), Some(mail)) ++ Json.obj("reservationSecret" -> regSec)

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

    "Refuse to register again with same secret" in {
      val path = basePath + "/account"
      val json = createUser(login +"a", pass) ++ Json.obj("reservationSecret" -> regSec)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
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

    "Refuse to reserve loginName that is an existing CameoIds and return alterantive" in {
      val path = basePath + "/account/check"
      val json = Json.obj("loginName" -> cameoIdExisting)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      Logger.debug("CONTENT: " + contentAsString(res))

      status(res) must equalTo(232)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "alternative").asOpt[String] must beSome(cameoIdExisting + "_1")
    }

    "Return a token" in {
      val path = basePath + "/token"

      val auth = "Basic " + new sun.misc.BASE64Encoder().encode((login + ":" + pass).getBytes)

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
      (data \ "phoneNumber" \ "value").asOpt[String] must beSome(cleanedTel)
    }
  }
}