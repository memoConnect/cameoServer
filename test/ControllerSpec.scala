package test

import play.api.test._
import play.api.libs.json.{ Json, JsObject }
import play.api.test.Helpers._
import play.api.test.FakeApplication
import testHelper.MockupFactory._
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import scala.concurrent.ExecutionContext
import play.api.{ GlobalSettings, Logger }
import services.DbAdminUtilities

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

    // fill db on startup
    val globalSettings = Some(new GlobalSettings() {
      override def onStart(app: play.api.Application) {
        DbAdminUtilities.loadFixtures()
      }
    })

    val additionalConfig = Map("mongodb.db" -> dbName,
      "mongo.init.loadOnStart" -> "false",
      "embed.mongo.enabled" -> "false"
    )

    // valid users in the inital Data: login;password;identityId;token
//    aC5LIRpX;password;shgofPlb0QlUxj80XHJL;GQhSOUDZ9Lm1En9bvgkh9Wb3vxgBZwamoxyundex
//    D2z3SOx3;password;DvrT60qr9cOncpwt4wFH;xlBdkdxFIzpnIdM32XirEGct5imtJUCsrrIoTOBf


    // Use the same FakeApplication for all tests, so the mongoConnection does not break
    lazy val app = FakeApplication(additionalConfiguration = additionalConfig, withGlobal = globalSettings)
    step(play.api.Play.start(app))

    val login = randomString(8)
    val pass = randomString(8)
    val mail = "e@mail.de"
    val tel = "+491234567890"
    var identityId = ""
    var token = ""
    var regSec = ""

    val token2 = "xlBdkdxFIzpnIdM32XirEGct5imtJUCsrrIoTOBf"

    "Refuse invalid Logins" in {

      val path = basePath + "/account/check"

      val logins = Seq("asdf", "asdfasdfasdfasdfasdfa", "..", ",asdf", "/asdf", "asdf#asdf", "asd£asdf", "<>", "\\")

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

    "Refuse to reserve reserved loginName and return alternative" in {
      val path = basePath + "/account/check"
      val json = Json.obj("loginName" -> login)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)

      val data = (contentAsJson(res) \ "error").as[JsObject]

      (data \ "alternative" \ "loginName").asOpt[String] must beSome(login + "_1")

      (data \ "alternative" \ "reservationSecret").asOpt[String] must beSome
    }

    "Refuse to claim reserved login without secret" in {
      val path = basePath + "/account"
      val json = createUser(login, pass)

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

    "Refuse duplicate Account names" in {
      val path = basePath + "/account"
      val json = createUser(login, pass)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
    }

    "Refuse to reserve existing loginName and return next alternative" in {
      val path = basePath + "/account/check"
      val json = Json.obj("loginName" -> login)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)

      val data = (contentAsJson(res) \ "error").as[JsObject]

      (data \ "alternative" \ "loginName").asOpt[String] must beSome(login + "_2")

      (data \ "alternative" \ "reservationSecret").asOpt[String] must beSome
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
      val path = basePath + "/identity/" + identityId + "?token=" + token

      val req = FakeRequest(GET, path)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "userKey").asOpt[String] must beSome
      (data \ "email" \ "value").asOpt[String] must beSome(mail)
      (data \ "phoneNumber" \ "value").asOpt[String] must beSome(tel)
    }

    "Edit an identity" in {

      val path = basePath + "/identity?token=" + token2

      val newPhone = "12345"
      val newMail = "asdfasdf"
      val newName = "new"

      val json = Json.obj("phoneNumber" -> newPhone, "email" -> newMail, "displayName" -> newName)

      val req = FakeRequest(PUT, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      Logger.debug("DATA" + data.toString)

      (data \ "phoneNumber" \ "value").asOpt[String] must beSome(newPhone)
      (data \ "phoneNumber" \ "isVerified").asOpt[Boolean] must beSome(false)
      (data \ "email" \ "value").asOpt[String] must beSome(newMail)
      (data \ "email" \ "isVerified").asOpt[Boolean] must beSome(false)
      (data \ "displayName").asOpt[String] must beSome(newName)
    }

    "drop the test database" in {
      ReactiveMongoPlugin.db.drop()(ExecutionContext.Implicits.global)
      1 === 1
    }

    step(play.api.Play.stop())
  }

}