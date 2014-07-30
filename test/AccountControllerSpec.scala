
import play.api.libs.json.JsArray
import play.api.test._
import play.api.libs.json.{ JsArray, Json, JsObject }
import play.api.test.FakeApplication
import play.api.test.Helpers._
import scala.Some
import testHelper.Stuff._
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import play.api.{ Play, Logger }
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

  val login = "superMoep"
  val login2 = "monsterMoep"
  val loginExternal = "superMonsterMoep"
  val pass = randomString(8)
  val displayName = "MOEP"
  val mail = validEmails(0)
  val tel = validPhoneNumbers(0)._1
  val cleanedTel = validPhoneNumbers(0)._2
  val displayName2 = "MOEP2"
  val mail2 = validEmails(1)
  val tel2 = validPhoneNumbers(1)._2
  var identityId = ""
  var token = ""
  var regSec = ""
  var regSec2 = ""

  "AccountController" should {

    "Refuse invalid Logins" in {
      val path = basePath + "/account/check"

      val logins = Seq("asdf", "asdfasdfasdfasdfasdfaasdfasdfasdfasdfasdf", "..", ",asdf", "/asdf", "asdf#asdf", "asd£asdf", "<>", "\\", "asdf.asdf.asdf", "asd@df")

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

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
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

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
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

    "Refuse to reserve reserved loginName with different capitalization" in {
      val path = basePath + "/account/check"
      val loginUpper = login.toUpperCase
      val json = Json.obj("loginName" -> loginUpper)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(232)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "alternative").asOpt[String] must beSome(loginUpper + "_1")
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

        val json = createUser(login, pass, Some(invalid), Some(mail)) ++ Json.obj("reservationSecret" -> regSec)

        val req = FakeRequest(POST, path).withJsonBody(json)
        val res = route(req).get

        status(res) must equalTo(BAD_REQUEST)
      }
    }

    "Refuse to register with wrong loginName for secret" in {
      val path = basePath + "/account"
      val json = createUser(login + "a", pass) ++ Json.obj("reservationSecret" -> regSec)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
    }

    "Create Account" in {
      val path = basePath + "/account"
      val json = createUser(login, pass, Some(tel), Some(mail)) ++
        Json.obj("reservationSecret" -> regSec) ++
        Json.obj("displayName" -> displayName)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
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
      val json = createUser(login + "a", pass) ++ Json.obj("reservationSecret" -> regSec)

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

      status(res) must equalTo(232)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "alternative").asOpt[String] must beSome(cameoIdExisting + "_1")
    }

    "Return a token" in {
      val path = basePath + "/token"

      val auth = "Basic " + new sun.misc.BASE64Encoder().encode((login + ":" + pass).getBytes)

      val req = FakeRequest(GET, path).withHeaders(("Authorization", auth))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      val tokenOpt = (data \ "token").asOpt[String]
      if (tokenOpt.isDefined) {
        token = tokenOpt.get
      }

      tokenOpt must beSome
    }

    "Return a token and ignore capitalization of loginName" in {
      val path = basePath + "/token"

      val auth = "Basic " + new sun.misc.BASE64Encoder().encode((login.toUpperCase + ":" + pass).getBytes)

      val req = FakeRequest(GET, path).withHeaders(("Authorization", auth))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "token").asOpt[String] must beSome
    }

    "Automatically create an identity for a new account" in {
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "userKey").asOpt[String] must beSome
      (data \ "cameoId").asOpt[String] must beSome(login)
      (data \ "displayName").asOpt[String] must beSome(displayName)
      (data \ "email" \ "value").asOpt[String] must beNone
      (data \ "phoneNumber" \ "value").asOpt[String] must beNone
    }

    var fileId = ""
    "automatically create avatar for new identity" in {
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "avatar").asOpt[String] must beSome
      fileId = (data \ "avatar").as[String]
      1 === 1
    }

    "check that avatar file meta exist" in {

      val path = basePath + "/file/" + fileId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(fileId)
      (data \ "chunks").asOpt[Seq[Int]] must beSome
      (data \ "chunks")(0).asOpt[Int] must beSome(0)
      (data \ "chunks")(1).asOpt[Int] must beNone
      (data \ "fileName").asOpt[String] must beSome("avatar.png")
      (data \ "fileSize").asOpt[Int] must beSome
      (data \ "fileType").asOpt[String] must beSome("image/png")
    }

    "check that avatar file chunk exists" in {
      val path = basePath + "/file/" + fileId + "/" + 0

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
      val raw = contentAsBytes(res)

      raw.length must beGreaterThan(100)
    }

    "automatically add support as contact" in {
      val path = basePath + "/contacts"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(1)

      (data(0) \ "identityId").asOpt[String] must beEqualTo(Play.configuration.getString("support.contact.identityId"))
    }

    var conversationId = ""
    "automatically add talk with support" in {
      val path = basePath + "/conversations"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      val conversations = (data \ "conversations").as[Seq[JsObject]]

      conversations.length must beEqualTo(1)
      (conversations(0) \ "subject").asOpt[String] must beEqualTo(Play.configuration.getString("support.conversation.subject"))

      val message = (conversations(0) \ "messages")(0).as[JsObject]
      (message \ "plain" \ "text").asOpt[String] must beEqualTo(Play.configuration.getString("support.conversation.body"))
    }

    var externalToken = ""
    "get purl object for external user" in {

      val path = basePath + "/purl/" + purlExtern2

      val req = FakeRequest(GET, path)
      val res = route(req).get


      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      externalToken = (data \ "token").as[String]

      1 === 1
    }

    var regSecExternal = ""
    "Reserve Login for external user" in {
      val path = basePath + "/account/check"
      val json = Json.obj("loginName" -> loginExternal)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      regSecExternal = (data \ "reservationSecret").as[String]

      1 === 1
    }

    "register user with token of external user" in {
      val path = basePath + "/account"
      val json = createUser(loginExternal, pass, Some(tel2), Some(mail2)) ++
        Json.obj("reservationSecret" -> regSecExternal) ++
        Json.obj("displayName" -> displayName2)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(externalToken))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      val identity = (data \ "identities")(0).as[JsObject]

      (identity \ "id").asOpt[String] must beSome(purlExtern2IdentitityId)
      (identity \ "phoneNumber" \ "value").asOpt[String] must beNone
      (identity \ "email" \ "value").asOpt[String] must beNone
      (identity \ "displayName").asOpt[String] must beSome(displayName2)
    }

    var purlExternIdentityToken = ""
    "get token of new account" in {

      val path = basePath + "/token"

      val auth = "Basic " + new sun.misc.BASE64Encoder().encode((loginExternal + ":" + pass).getBytes)

      val req = FakeRequest(GET, path).withHeaders(("Authorization", auth))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      (contentAsJson(res) \ "data" \ "token").asOpt[String] must beSome
      purlExternIdentityToken = (contentAsJson(res) \ "data" \ "token").as[String]

      1 === 1
    }

    "get identity of new account" in {

      val path = basePath + "/identity/" + purlExtern2IdentitityId

      val req = FakeRequest(GET, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(purlExtern2IdentitityId)
      (data \ "cameoId").asOpt[String] must beSome(loginExternal)
      (data \ "avatar").asOpt[String] must beSome
      (data \ "displayName").asOpt[String] must beSome(displayName2)
    }

    "identity should have sender as contact" in {

      val path = basePath + "/contacts"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(purlExternIdentityToken))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(2)

      data.find(js => (js \ "identityId").asOpt[String].equals(Some(identityExisting2))) must beSome
    }

    "Reserve another login" in {
      val path = basePath + "/account/check"
      val json = Json.obj("loginName" -> (loginExternal + "moep"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      regSecExternal = (data \ "reservationSecret").as[String]

      1 === 1
    }

    "refuse to register with token of internal user" in {
      val path = basePath + "/account"
      val json = createUser(loginExternal, pass, Some(tel2), Some(mail2)) ++
        Json.obj("reservationSecret" -> regSecExternal) ++
        Json.obj("displayName" -> displayName2)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
    }

    val newPhoneNumber = "+49123456"
    val newEmail = "asdfasdf@moep.de"
    val newPassword = "asdfasdfasdf"

    "update phoneNumber and email of account" in {

      val path = basePath + "/account"
      val json = Json.obj("phoneNumber" -> newPhoneNumber, "email" -> newEmail)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "account should contain new values" in {
      val path = basePath + "/account"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "loginName").asOpt[String] must beSome
      (data \ "identities").asOpt[Seq[JsObject]] must beSome
      (data \ "email" \ "value").asOpt[String] must beSome(newEmail)
      (data \ "phoneNumber" \ "value").asOpt[String] must beSome(newPhoneNumber)
    }

    "get token with old password" in {
      val path = basePath + "/token"

      val auth = "Basic " + new sun.misc.BASE64Encoder().encode((loginExisting2 + ":" + password).getBytes)

      val req = FakeRequest(GET, path).withHeaders(("Authorization", auth))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "update account password" in {
      val path = basePath + "/account"
      val json = Json.obj("password" -> newPassword)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "refuse login with old password" in {
      val path = basePath + "/token"

      val auth = "Basic " + new sun.misc.BASE64Encoder().encode((loginExisting2 + ":" + password).getBytes)

      val req = FakeRequest(GET, path).withHeaders(("Authorization", auth))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(UNAUTHORIZED)

    }

    "allow login with new password" in {
      val path = basePath + "/token"

      val auth = "Basic " + new sun.misc.BASE64Encoder().encode((loginExisting2 + ":" + newPassword).getBytes)

      val req = FakeRequest(GET, path).withHeaders(("Authorization", auth))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

    }
  }
}