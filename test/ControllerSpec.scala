import play.api.libs.json.JsArray
import play.api.libs.json.JsObject
import play.api.test._
import play.api.libs.json.{ JsArray, Json, JsObject }
import play.api.test.FakeApplication
import play.api.test.Helpers._
import play.api.test.FakeApplication
import scala.Some
import testHelper.MockupFactory._
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import scala.concurrent.ExecutionContext
import play.api.{ GlobalSettings, Logger }
import helper.DbAdminUtilities
import testHelper.MockupFactory
import org.specs2.mutable._

class ControllerSpec extends Specification {

  val basePath = "/api/v1"
  val dbName = "cameo_test"

  sequential

  "Controller" should {

    val additionalConfig = Map("mongodb.db" -> dbName)

    // valid users in the inital Data: login;password;identityId;token
    //    BMeSfHXQ;password;N2HKgBdxxnWBGxlYY7Dn;viRlhZZ1VDAhqcgrljvfzEXCwKj0B2dyAKw5suFZ
    //    2VqTftqh;password;g9PWZY7xKNbeCO6LPNnx;hUbODA2qkVo2JF7YdEYVXe4NaHd82x6rvxxBxXbo

    // test user on dev.cameo.io
    // r1Zhpq8e;password;NyGAvBnLeR3mLEYdofgf;lFFkssj7gE4uTGSZlPlolp82Ozp3fWnOkQEFYO6k


    // Use the same FakeApplication for all tests, so the mongoConnection does not break
    lazy val app = FakeApplication(additionalConfiguration = additionalConfig)
    step(play.api.Play.start(app))

    val login = randomString(8)
    val login2 = randomString(8)
    val pass = randomString(8)
    val mail = "e@mail.de"
    val tel = "+491234567890"
    var identityId = ""
    var token = ""
    var regSec = ""
    var regSec2 = ""
    var cidNew = ""
    var identityOf10thContact = ""
    var idOf10thContact = ""
    var contactId = ""
    val newContactMail = "test@bjrm.de"
    val newContactTel = "+4561233"
    val newContactName = "foobar"
    val cidExisting = "rQHQZHv4ARDXRmnEzJ92"
    val cidOther = "2GOdNSfdPMavyl95KUah"
    val identityExisting = "g9PWZY7xKNbeCO6LPNnx"
    val identityExisting2 = "N2HKgBdxxnWBGxlYY7Dn"
    val validRecipients = Seq("6iOuCefN12ma0wF7QxR5", "dKeg67XtSNBCFMq8WQor")
    val recipientMemberOfConversation = "Tya0cZiaYFhFOBS2RNP1"
    val purlExtern = "MSaKlj4hJP"
    val purlExtern2 = "PkFWPuCiBB"
    val purlExtern3 = "agirsrEN3j"
    val purlExternIdentitityId = "GhEWGfy3Jqx8BRP1pITO"
    val purlIntern = "V3Ml6hzqX8"
    val purlIntern2 = "u02iLiIeQu"
    val purlConversationId = "OM9QeJ4RfJcdscyo52g4"

    val token2 = "hUbODA2qkVo2JF7YdEYVXe4NaHd82x6rvxxBxXbo"
    val token3 = "viRlhZZ1VDAhqcgrljvfzEXCwKj0B2dyAKw5suFZ"

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

    "Get the identity behind a token" in {
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

    val newPhone = "12345"
    val newMail = "asdfasdf"
    val newName = "newNameasdfasdf"

    "Edit an identity" in {

      val path = basePath + "/identity"

      val json = Json.obj("phoneNumber" -> newPhone, "email" -> newMail, "displayName" -> newName)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

    }

    "check if identity was edited" in {

      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "phoneNumber" \ "value").asOpt[String] must beSome(newPhone)
      (data \ "phoneNumber" \ "isVerified").asOpt[Boolean] must beSome(false)
      (data \ "email" \ "value").asOpt[String] must beSome(newMail)
      (data \ "email" \ "isVerified").asOpt[Boolean] must beSome(false)
      (data \ "displayName").asOpt[String] must beSome(newName)

    }

    "Search for an CameoId" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> login, "fields" -> Seq("cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "cameoId").asOpt[String] must beSome(login)
      (data(0) \ "id").asOpt[String] must beSome(identityId)

      data.length must beEqualTo(1)
    }

    "Search for an CameoId with substring" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> login.substring(0, 4), "fields" -> Seq("cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "cameoId").asOpt[String] must beSome(login)
      (data(0) \ "id").asOpt[String] must beSome(identityId)

      data.length must beEqualTo(1)
    }

    "Refuse to Search for an CameoId if the search term is too short" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> "abc", "fields" -> Seq("cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
    }

    "Find nothing for non-existing CameoIds" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> "moep", "fields" -> Seq("cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[String] = (contentAsJson(res) \ "data").as[Seq[String]]

      data.length must beEqualTo(0)
    }

    "Search for a DisplayName" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> newName, "fields" -> Seq("displayName"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "id").asOpt[String] must beSome(identityExisting)

      data.length must beEqualTo(1)
    }

    "Search for DisplayName with substring" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> newName.substring(2,6), "fields" -> Seq("displayName"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "id").asOpt[String] must beSome(identityExisting)

      data.length must beEqualTo(1)
    }

    "Find nothing for non-existing DisplayNames" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> "moep", "fields" -> Seq("displayName"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[String] = (contentAsJson(res) \ "data").as[Seq[String]]

      data.length must beEqualTo(0)
    }

    "Search for DisplayName or CameoId 1 " in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> newName.substring(2,6), "fields" -> Seq("displayName","cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "id").asOpt[String] must beSome(identityExisting)

      data.length must beEqualTo(1)
    }

    "Search for DisplayName or CameoId 2 " in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> login.substring(0, 4), "fields" -> Seq("cameoId","displayName"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "cameoId").asOpt[String] must beSome(login)
      (data(0) \ "id").asOpt[String] must beSome(identityId)

      data.length must beEqualTo(1)
    }


    "Create a new conversation with subject" in {
      val path = basePath + "/conversation"

      val subject = "test subject"
      val json = Json.obj("subject" -> subject)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      val recipient = (data \ "recipients")(0).as[JsObject]
      (recipient \ "identityId").asOpt[String] must beSome(identityExisting)
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[String] must beSome
      (data \ "lastUpdated").asOpt[String] must beSome
      (data \ "subject").asOpt[String] must beSome(subject)
    }

    "Create a new conversation without subject" in {
      val path = basePath + "/conversation"

      val req = FakeRequest(POST, path).withJsonBody(Json.obj()).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      cidNew = (data \ "id").as[String]
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      val recipient = (data \ "recipients")(0).as[JsObject]
      (recipient \ "identityId").asOpt[String] must beSome(identityExisting)
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[String] must beSome
      (data \ "lastUpdated").asOpt[String] must beSome
      (data \ "subject").asOpt[String] must beNone

    }

    "Get the created conversation" in {
      val path = basePath + "/conversation/" + cidNew

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      val r: JsObject = (data \ "recipients")(0).as[JsObject]
      (r \ "identityId").asOpt[String] must beSome(identityExisting)
      (r \ "identity" \ "displayName").asOpt[String] must beSome(newName)
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[String] must beSome
      (data \ "lastUpdated").asOpt[String] must beSome
      (data \ "subject").asOpt[String] must beNone
    }

    val newSubject = "moep"

    "Edit subject of an conversation" in {
      val path = basePath + "/conversation/" + cidExisting

      val json = Json.obj("subject" -> newSubject)

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(token2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "Check if subject has changed" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "subject").asOpt[String] must beSome(newSubject)
    }

    "Refuse non-member to edit subject of an conversation" in {
      val path = basePath + "/conversation/" + cidExisting

      val json = Json.obj("subject" -> newSubject)

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(token3)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "Get an existing conversation with messages" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      val r: JsObject = (data \ "recipients")(0).as[JsObject]
      (r \ "identityId").asOpt[String] must beSome(identityExisting)
      (r \ "identity" \ "displayName").asOpt[String] must beSome(newName)
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      val m: Seq[JsObject] = (data \ "messages").as[Seq[JsObject]]
      m.length must beEqualTo(100)
      (data \ "created").asOpt[String] must beSome
      (data \ "lastUpdated").asOpt[String] must beSome
    }

    "get conversation summary" in {
      val path = basePath + "/conversation/" + cidExisting + "/summary"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "numberOfMessages").asOpt[Int] must beSome(100)
      (data \ "lastUpdated").asOpt[String] must beSome
      (data \ "lastMessage").asOpt[String] must beSome
      (data \ "subject").asOpt[String] must beSome

    }

    "get conversations of user" in {
      val path = basePath + "/conversations"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(13)

      // the list should consist of conversation summaries
      data.map {
        c =>
          (c \ "id").asOpt[String] must beSome
          (c \ "numberOfMessages").asOpt[Int] must beSome
          (c \ "lastUpdated").asOpt[String] must beSome
          (c \ "lastMessage").asOpt[String] must beSome
      }
      // check if it contains ids
      data.exists(c => (c \ "id").asOpt[String].equals(Some(cidNew))) must beTrue
      data.exists(c => (c \ "id").asOpt[String].equals(Some(cidExisting))) must beTrue
    }

    "add recipient to conversation" in {
      val path = basePath + "/conversation/" + cidExisting + "/recipient"

      val json = Json.obj("recipients" -> validRecipients)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "refuse to add duplicate recipient to conversation" in {
      val path = basePath + "/conversation/" + cidExisting + "/recipient"

      val json = Json.obj("recipients" -> Seq(validRecipients(0)))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(232)
    }

    "conversation should contain new recipients" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      val recipients = (data \ "recipients").as[Seq[JsObject]]

      recipients.length must beEqualTo(4)

      recipients.exists(r => (r \ "identityId").as[String].equals(validRecipients(0))) must beTrue
      recipients.exists(r => (r \ "identityId").as[String].equals(validRecipients(1))) must beTrue
    }

    "refuse to add non-existing recipients" in {
      val path = basePath + "/conversation/" + cidExisting + "/recipient"

      val json = Json.obj("recipients" -> Seq("asdf"))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)

      contentAsString(res) must contain("\"res\":\"KO\"")
      contentAsString(res) must contain("invalid")
    }

    "refuse non-members to add recipients to conversation" in {

      val path = basePath + "/conversation/" + cidOther + "/recipient"

      val json = Json.obj("recipients" -> validRecipients)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)

      contentAsString(res) must contain("\"res\":\"KO\"")
      contentAsString(res) must contain("identity is not a member of the conversation")
    }

    "delete recipient from conversation" in {

      val path = basePath + "/conversation/" + cidExisting + "/recipient/" + validRecipients(0)

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "refuse non-members to delete recipient from conversation" in {

      val path = basePath + "/conversation/" + cidExisting + "/recipient/" + validRecipients(0)

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(token3))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "conversation must not contain deleted recipient" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      val recipients = (data \ "recipients").as[Seq[JsObject]]

      Logger.debug("740:" + recipients)

      recipients.length must beEqualTo(3)

      recipients.exists(r => (r \ "identityId").as[String].equals(recipientMemberOfConversation)) must beFalse
    }

    var messageId = ""

    "add message to conversation" in {
      val path = basePath + "/conversation/" + cidExisting + "/message"

      val json = Json.obj("messageBody" -> "wir rocken")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      messageId = (data \ "id").as[String]
      (data \ "messageBody").asOpt[String] must beSome("wir rocken")
      (data \ "messageStatus").asOpt[Seq[JsObject]] must beSome
      (data \ "fromIdentity").asOpt[String] must beSome
      (data \ "created").asOpt[String] must beSome
    }

    "get single message" in {
      val path = basePath + "/message/" + messageId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      messageId = (data \ "id").as[String]
      (data \ "messageBody").asOpt[String] must beSome("wir rocken")
      (data \ "messageStatus").asOpt[Seq[JsObject]] must beSome
      (data \ "fromIdentity").asOpt[String] must beSome
      (data \ "created").asOpt[String] must beSome
    }

    "refuse non-members to send message to conversation" in {

      val path = basePath + "/conversation/" + cidExisting + "/message"

      val json = Json.obj("messageBody" -> "wir rocken")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token3)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)

    }

    "refuse non-members to get single message" in {
      val path = basePath + "/message/" + messageId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token3))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    val encryptedKey = "foobarbaz!"

    "add encrypted key to recipient in conversation" in {

      val path = basePath + "/conversation/" + cidExisting + "/recipient/" + validRecipients(0)

      val json = Json.obj("encryptedKey" -> encryptedKey)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "return the encrypted key" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      val recipients = (data \ "recipients").as[Seq[JsObject]]

      val recipient0 = recipients.filter(r => (r \ "identityId").as[String].equals(validRecipients(0)))(0)
      (recipient0 \ "encryptedKey").asOpt[String] must beSome(encryptedKey)

      val recipient1 = recipients.filter(r => (r \ "identityId").as[String].equals(validRecipients(1)))(0)
      (recipient1 \ "encryptedKey").asOpt[String] must beNone
    }

    "refuse non-members to add encrypted keys to recipient" in {

      val path = basePath + "/conversation/" + cidExisting + "/recipient/" + validRecipients(0)

      val json = Json.obj("encryptedKey" -> encryptedKey)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token3)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "get all contacts" in {
      val path = basePath + "/contacts"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(44)

      val contact = data(10)
      (contact \ "groups")(0).asOpt[String] must beSome("group1")
      (contact \ "identityId").asOpt[String] must beSome
      identityOf10thContact = (contact \ "identityId").as[String]
      (contact \ "id").asOpt[String] must beSome
      idOf10thContact = (contact \ "id").as[String]
      (contact \ "identity" \ "displayName").asOpt[String] must beSome
    }

    "get all contacts with offset" in {

      val path = basePath + "/contacts?offset=10"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(34)
    }

    "get all contacts with limit" in {

      val path = basePath + "/contacts?limit=20"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(20)
    }

    "get all contacts with limit and offset" in {
      val path = basePath + "/contacts?offset=10&limit=20"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(20)
      (data(0) \ "identityId").asOpt[String] must beSome(identityOf10thContact)
    }

    "add internal contact" in {
      val path = basePath + "/contact"

      val json = Json.obj("groups" -> Seq("group3", "group1"), "identityId" -> identityId)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      contactId = (data \ "id").as[String]
      (data \ "groups")(0).asOpt[String] must beSome("group3")
      (data \ "groups")(1).asOpt[String] must beSome("group1")
      (data \ "identityId").asOpt[String] must beSome(identityId)
      (data \ "contactType").asOpt[String] must beSome("internal")

    }

    "refuse to add internal contact with invalid identity" in {
      val path = basePath + "/contact"

      val json = Json.obj("groups" -> Seq("group3", "group1"), "identityId" -> "asdf")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
    }

    "get the new internal contact" in {

      val path = basePath + "/contact/" + contactId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "identityId").asOpt[String] must beSome(identityId)
    }

    "edit groups of internal contact" in {

      val path = basePath + "/contact/" + contactId

      val newGroups = Seq("group1", "group4")
      val json = Json.obj("groups" -> newGroups)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "refuse to edit mail of internal contact" in {
      val path = basePath + "/contact/" + contactId

      val newMail = "new@mail.de"
      val json = Json.obj("email" -> newMail)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "refuse to edit phoneNumber of internal contact" in {
      val path = basePath + "/contact/" + contactId

      val newPhone = "+142536"
      val json = Json.obj("phoneNumber" -> newPhone)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "refuse to edit DisplayName of internal contact" in {
      val path = basePath + "/contact/" + contactId

      val newName = "fail"
      val json = Json.obj("displayName" -> newName)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "add external contact" in {
      val path = basePath + "/contact"

      val mail = "some@mail.com"
      val tel = "+123456789123"
      val name = "foo"
      val json = Json.obj("groups" -> Seq("group1", "group2"),
        "identity" -> Json.obj("email" -> mail, "phoneNumber" -> tel, "displayName" -> name))

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      contactId = (data \ "id").as[String]
      (data \ "groups")(0).asOpt[String] must beSome("group1")
      (data \ "groups")(1).asOpt[String] must beSome("group2")
      (data \ "contactType").asOpt[String] must beSome("external")
      (data \ "identity" \ "email" \ "value").asOpt[String] must beSome(mail)
      (data \ "identity" \ "phoneNumber" \ "value").asOpt[String] must beSome(tel)
      (data \ "identity" \ "displayName").asOpt[String] must beSome(name)

    }

    "get the new external contact" in {

      val path = basePath + "/contact/" + contactId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(contactId)
      (data \ "groups")(0).asOpt[String] must beSome("group1")
      (data \ "groups")(1).asOpt[String] must beSome("group2")
      (data \ "contactType").asOpt[String] must beSome("external")
      (data \ "identity" \ "email" \ "value").asOpt[String] must beSome
      (data \ "identity" \ "phoneNumber" \ "value").asOpt[String] must beSome
      (data \ "identity" \ "displayName").asOpt[String] must beSome
    }

    "edit groups of external contact" in {

      val path = basePath + "/contact/" + contactId

      val newGroups = Seq("group1", "group3")
      val json = Json.obj("groups" -> newGroups)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "edit details of new contact" in {
      val path = basePath + "/contact/" + contactId

      val json = Json.obj("email" -> newContactMail, "phoneNumber" -> newContactTel, "displayName" -> newContactName)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "get all contact groups" in {
      val path = basePath + "/contact-groups"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[String]]

      data.find(_.equals("group1")) aka "contain group 1" must beSome
      data.find(_.equals("group2")) aka "contain group 2" must beSome
      data.find(_.equals("group3")) aka "contain group 3" must beSome
      data.find(_.equals("group4")) aka "contain group 4" must beSome
    }

    "get single group" in {
      val path = basePath + "/contact-group/group1"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(24)

    }

    "get group with created internal contact" in {
      val path = basePath + "/contact-group/group4"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(1)

      (data(0) \ "identityId").asOpt[String] must beSome(identityId)
    }

    "get group with created external contact" in {
      val path = basePath + "/contact-group/group3"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(1)

      (data(0) \ "id").asOpt[String] must beSome(contactId)
      (data(0) \ "groups")(0).asOpt[String] must beSome("group1")
      (data(0) \ "groups")(1).asOpt[String] must beSome("group3")
      (data(0) \ "contactType").asOpt[String] must beSome("external")
      (data(0) \ "identity" \ "email" \ "value").asOpt[String] must beSome(newContactMail)
      (data(0) \ "identity" \ "phoneNumber" \ "value").asOpt[String] must beSome(newContactTel)
      (data(0) \ "identity" \ "displayName").asOpt[String] must beSome(newContactName)
    }

    "delete Contact" in {
      val path = basePath + "/contact/" + idOf10thContact

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "check deletion" in {
      val path = basePath + "/contact/" + idOf10thContact

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(NOT_FOUND)
    }

    "refuse to delete non-existing contact" in {
      val path = basePath + "/contact/asdfasdf"

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(NOT_FOUND)
    }

    "send FriendRequest with identityId" in {
      val path = basePath + "/friendRequest"

      val json = Json.obj("identityId" -> identityId)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "refuse to send FriendRequest to invalid identityId" in {
      val path = basePath + "/friendRequest"

      val json = Json.obj("identityId" -> "asdfasdf")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(NOT_FOUND)
    }

    "send FriendRequest with cameoId" in {
      val path = basePath + "/friendRequest"

      val json = Json.obj("cameoId" -> login)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token3)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "refuse to send FriendRequest to invalid cameoId" in {
      val path = basePath + "/friendRequest"

      val json = Json.obj("cameoId" -> "pups")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token3)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(NOT_FOUND)
    }

    "get friendRequests" in {
      val path = basePath + "/friendRequests"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(2)

      (data(0) \ "id").asOpt[String] must beSome(identityExisting)
      (data(1) \ "id").asOpt[String] must beSome(identityExisting2)
    }

    "accept FriendRequest" in {
      val path = basePath + "/friendRequest/answer"

      val json = Json.obj("answerType" -> "accept", "identityId" -> identityExisting2)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "check if contact was added to sender" in {
      val path = basePath + "/contacts"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.find(c => (c \ "identityId").as[String].equals(identityExisting2)) must beSome

    }

    "check if contact was added to receiver" in {
      val path = basePath + "/contacts"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token3))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.find(c => (c \ "identityId").as[String].equals(identityId)) must beSome
    }

    "reject FriendRequest" in {
      val path = basePath + "/friendRequest/answer"

      val json = Json.obj("answerType" -> "reject", "identityId" -> identityExisting)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "check if friendRequests are gone" in {
      val path = basePath + "/friendRequests"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(0)
    }

    val pubKey = "asdfasdfasdf"
    val pubKeyName= "moep"
    var pubKeyId = ""
    val newPubKey = "woops"
    val newPubKeyName= "poem"

    val pubKey2 = "asdfasdfasdf2"
    val pubKeyName2= "moep2"
    var pubKeyId2 = ""

    "add public key to identity" in {
      val path = basePath + "/identity/publicKey"

      val json = Json.obj("name" -> pubKeyName, "key" -> pubKey)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      pubKeyId = (data \ "id").as[String]
      (data \ "name").asOpt[String] must beSome(pubKeyName)
      (data \ "key").asOpt[String] must beSome(pubKey)
    }

    "add another public key to identity" in {
      val path = basePath + "/identity/publicKey"

      val json = Json.obj("name" -> pubKeyName2, "key" -> pubKey2)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      pubKeyId2 = (data \ "id").as[String]
      (data \ "name").asOpt[String] must beSome(pubKeyName2)
      (data \ "key").asOpt[String] must beSome(pubKey2)
    }

    "check if identity contains both added public keys" in {

      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "publicKeys").asOpt[Seq[JsObject]] must beSome
      val pubKeys = (data \ "publicKeys").as[Seq[JsObject]]

      pubKeys.length must beEqualTo(2)

      val key1: JsObject = pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId)).get
      (key1 \ "id").asOpt[String] must beSome(pubKeyId)
      (key1 \ "name").asOpt[String] must beSome(pubKeyName)
      (key1 \ "key").asOpt[String] must beSome(pubKey)

      val key2: JsObject = pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId2)).get
      (key2 \ "id").asOpt[String] must beSome(pubKeyId2)
      (key2 \ "name").asOpt[String] must beSome(pubKeyName2)
      (key2 \ "key").asOpt[String] must beSome(pubKey2)
    }

    "edit name of public key" in {

      val path = basePath + "/identity/publicKey/" + pubKeyId

      val json = Json.obj("name" -> newPubKeyName)

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(token2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "edit public key" in {

      val path = basePath + "/identity/publicKey/" + pubKeyId

      val json = Json.obj("key" -> newPubKey)

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(token2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "check if public key was edited" in {
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "publicKeys").asOpt[Seq[JsObject]] must beSome
      val pubKeys = (data \ "publicKeys").as[Seq[JsObject]]

      pubKeys.length must beEqualTo(2)

      val key1: JsObject = pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId)).get
      (key1 \ "id").asOpt[String] must beSome(pubKeyId)
      (key1 \ "name").asOpt[String] must beSome(newPubKeyName)
      (key1 \ "key").asOpt[String] must beSome(newPubKey)

      val key2: JsObject = pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId2)).get
      (key2 \ "id").asOpt[String] must beSome(pubKeyId2)
      (key2 \ "name").asOpt[String] must beSome(pubKeyName2)
      (key2 \ "key").asOpt[String] must beSome(pubKey2)
    }

    "delete a public key" in {
      val path = basePath + "/identity/publicKey/" + pubKeyId

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "check if key was deleted" in {
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "publicKeys").asOpt[Seq[JsObject]] must beSome
      val pubKeys = (data \ "publicKeys").as[Seq[JsObject]]

      pubKeys.length must beEqualTo(1)

      pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId)) must beNone

      val key2: JsObject = pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId2)).get
      (key2 \ "id").asOpt[String] must beSome(pubKeyId2)
      (key2 \ "name").asOpt[String] must beSome(pubKeyName2)
      (key2 \ "key").asOpt[String] must beSome(pubKey2)
    }

    val fileName = "some_name.pdf"
    val fileType = "some_type"
    val fileSize = 1234567
    var fileId = ""



    var purlExternToken = ""

    "get purl object for external user" in {

      val path = basePath + "/purl/" + purlExtern

      val req = FakeRequest(GET, path)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "conversation").asOpt[JsObject] must beSome
      val conversation = (data \ "conversation").as[JsObject]
      (data \ "identity").asOpt[JsObject] must beSome
      val identity = (data \ "identity").as[JsObject]
      (data \ "token").asOpt[String] must beSome
      purlExternToken = (data \ "token").as[String]

      (conversation \ "id").asOpt[String] must beSome(purlConversationId)
      (identity \ "id").asOpt[String] must beSome(purlExternIdentitityId)
    }

    "get purl object for external user with token" in {

      val path = basePath + "/purl/" + purlExtern

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(purlExternToken))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "conversation").asOpt[JsObject] must beSome
      val conversation = (data \ "conversation").as[JsObject]
      (conversation \ "id").asOpt[String] must beSome(purlConversationId)
      (data \ "identity").asOpt[JsObject] must beSome
      val identity = (data \ "identity").as[JsObject]
      (identity \ "id").asOpt[String] must beSome(purlExternIdentitityId)
      (data \ "token").asOpt[String] must beNone
    }

    "refuse to return purl object for external user with wrong token" in {
      val path = basePath + "/purl/" + purlExtern

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token3))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "get purl object of internal user with token" in {

      val path = basePath + "/purl/" + purlIntern

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token3))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "conversation").asOpt[JsObject] must beSome
      val conversation = (data \ "conversation").as[JsObject]
      (conversation \ "id").asOpt[String] must beSome(purlConversationId)

      (data \ "identity").asOpt[JsObject] must beSome
      val identity = (data \ "identity").as[JsObject]
      (identity \ "id").asOpt[String] must beSome(identityExisting2)

      (data \ "token").asOpt[String] must beNone
    }

    "refuse to get purl object of internal user with wrong token" in {

      val path = basePath + "/purl/" + purlIntern

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)

    }

    "refuse to get purl object of internal user with token of other member of the conversation" in {

      val path = basePath + "/purl/" + purlIntern

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(purlExternToken))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "refuse to get purl object of internal user without token" in {

      val path = basePath + "/purl/" + purlIntern

      val req = FakeRequest(GET, path)
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }


    "drop the test database" in {
      ReactiveMongoPlugin.db.drop()(ExecutionContext.Implicits.global)
      1 === 1
    }

    step(play.api.Play.stop())

  }

}