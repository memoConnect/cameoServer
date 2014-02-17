package test

import play.api.test._
import play.api.libs.json.{ JsArray, Json, JsObject }
import play.api.test.Helpers._
import play.api.test.FakeApplication
import testHelper.MockupFactory._
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import scala.concurrent.ExecutionContext
import play.api.{ GlobalSettings, Logger }
import helper.DbAdminUtilities

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
    //    BMeSfHXQ;password;N2HKgBdxxnWBGxlYY7Dn;viRlhZZ1VDAhqcgrljvfzEXCwKj0B2dyAKw5suFZ
    //    2VqTftqh;password;g9PWZY7xKNbeCO6LPNnx;hUbODA2qkVo2JF7YdEYVXe4NaHd82x6rvxxBxXbo

    // Use the same FakeApplication for all tests, so the mongoConnection does not break
    lazy val app = FakeApplication(additionalConfiguration = additionalConfig, withGlobal = globalSettings)
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
    val newContactMail = "new@mail.foo"
    val newContactTel = "+4561233"
    val newContactName = "foobar"
    val cidExisting = "rQHQZHv4ARDXRmnEzJ92"
    val cidOther = "2GOdNSfdPMavyl95KUah"
    val identityExisting = "g9PWZY7xKNbeCO6LPNnx"
    val identityExisting2 = "N2HKgBdxxnWBGxlYY7Dn"
    val validRecipients = Seq("6iOuCefN12ma0wF7QxR5", "dKeg67XtSNBCFMq8WQor")
    val recipientMemberOfConversation = "Tya0cZiaYFhFOBS2RNP1"

    val token2 = "hUbODA2qkVo2JF7YdEYVXe4NaHd82x6rvxxBxXbo"

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
      val path = basePath + "/identity/" + identityId

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

    "Edit an identity" in {

      val path = basePath + "/identity"

      val newPhone = "12345"
      val newMail = "asdfasdf"
      val newName = "new"

      val json = Json.obj("phoneNumber" -> newPhone, "email" -> newMail, "displayName" -> newName)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(token2))
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

      val json = Json.obj("cameoId" -> login)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[String] = (contentAsJson(res) \ "data").as[Seq[String]]

      data(0) must beEqualTo(login)

      data.length must beEqualTo(1)
    }

    "Search for an CameoId with substring" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("cameoId" -> login.substring(0, 4))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[String] = (contentAsJson(res) \ "data").as[Seq[String]]

      data(0) must beEqualTo(login)

      data.length must beEqualTo(1)
    }

    "Refuse to Search for an CameoId if the search term is too short" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("cameoId" -> "abc")

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
    }

    "Find nothing for non-existing CameoIds" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("cameoId" -> "abcabc")

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[String] = (contentAsJson(res) \ "data").as[Seq[String]]

      data.length must beEqualTo(0)
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
      (data \ "recipients")(0).asOpt[String] must beSome(identityExisting)
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
      (data \ "recipients")(0).asOpt[String] must beSome(identityExisting)
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
      (r \ "id").asOpt[String] must beSome(identityExisting)
      (r \ "displayName").asOpt[String] must beSome("new")
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[String] must beSome
      (data \ "lastUpdated").asOpt[String] must beSome
      (data \ "subject").asOpt[String] must beNone
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
      (r \ "id").asOpt[String] must beSome(identityExisting)
      (r \ "displayName").asOpt[String] must beSome("new")
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      val m: Seq[JsObject] = (data \ "messages").as[Seq[JsObject]]
      m.length must beEqualTo(100)
      (data \ "created").asOpt[String] must beSome
      (data \ "lastUpdated").asOpt[String] must beSome
      (data \ "subject").asOpt[String] must beSome("some 1337 subject hqDBv")
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

      data.length must beEqualTo(12)

      // the list should consist of conversation summaries
      data.map {
        c =>
          (c \ "id").asOpt[String] must beSome
          (c \ "numberOfMessages").asOpt[Int] must beSome
          (c \ "lastUpdated").asOpt[String] must beSome
          (c \ "lastMessage").asOpt[String] must beSome
      }
      // check if it contains ids
      data.count(c => (c \ "id").asOpt[String] == Some(cidNew)) must beEqualTo(1)
      data.count(c => (c \ "id").asOpt[String] == Some(cidExisting)) must beEqualTo(1)
    }

    "add recipient to conversation" in {
      val path = basePath + "/conversation/" + cidExisting + "/recipient"

      val json = Json.obj("recipients" -> validRecipients)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "conversation should contain new recipients" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      val recipients = (data \ "recipients").as[Seq[JsObject]]

      recipients.count(r => (r \ "id").as[String] == validRecipients(0)) must beEqualTo(1)
      recipients.count(r => (r \ "id").as[String] == validRecipients(1)) must beEqualTo(1)
    }

    "refuse to add non-existing recipients" in {
      val path = basePath + "/conversation/" + cidExisting + "/recipient"

      val json = Json.obj("recipients" -> Seq("asdf"))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)

      contentAsString(res) must contain("\"res\":\"KO\"")
      contentAsString(res) must contain("at least one recipientId is invalid")
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

      val path = basePath + "/conversation/" + cidExisting + "/recipient/" + recipientMemberOfConversation

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "conversation should not contain deleted recipient" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      val recipients = (data \ "recipients").as[Seq[JsObject]]

      recipients.count(r => (r \ "id").as[String] == recipientMemberOfConversation) must beEqualTo(0)
    }

    "get all contacts" in {
      val path = basePath + "/contacts"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(44)

      val contact = data(10)
      ( contact \ "groups")(0).asOpt[String] must beSome("group1")
      ( contact \ "identityId").asOpt[String] must beSome
      identityOf10thContact = ( contact \ "identityId").as[String]
      ( contact \ "id").asOpt[String] must beSome
      idOf10thContact = ( contact \ "id").as[String]
      ( contact \ "identity" \ "displayName").asOpt[String] must beSome
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

      val json = Json.obj("groups" -> Seq("group3","group1"), "identityId" -> identityId )

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      ( data \ "id").asOpt[String] must beSome
      contactId = (data \ "id").as[String]
      (data \ "groups")(0).asOpt[String] must beSome("group3")
      (data \ "groups")(1).asOpt[String] must beSome("group1")
      (data \ "identityId").asOpt[String] must beSome(identityId)
      (data \ "contactType").asOpt[String] must beSome("internal")

    }

    "refuse to add internal contact with invalid identity" in {
      val path = basePath + "/contact"

      val json = Json.obj("groups" -> Seq("group3","group1"), "identityId" -> "asdf" )

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

      val newGroups = Seq("group1","group4")
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
      val json = Json.obj("groups" -> Seq("group1","group2"),
        "identity" -> Json.obj("email" -> mail, "phoneNumber" -> tel, "displayName" -> name ) )

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(token2)).withJsonBody(json)
      val res = route(req).get

      Logger.debug("DASDF: " + contentAsString(res))

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      ( data \ "id").asOpt[String] must beSome
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

      ( data \ "id").asOpt[String] must beSome(contactId)
      (data \ "groups")(0).asOpt[String] must beSome("group1")
      (data \ "groups")(1).asOpt[String] must beSome("group2")
      (data \ "contactType").asOpt[String] must beSome("external")
      (data \ "identity" \ "email" \ "value").asOpt[String] must beSome
      (data \ "identity" \ "phoneNumber" \ "value").asOpt[String] must beSome
      (data \ "identity" \ "displayName").asOpt[String] must beSome
    }

    "edit groups of external contact" in {

      val path = basePath + "/contact/" + contactId

      val newGroups = Seq("group1","group3")
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

      Logger.debug("ID: " + idOf10thContact)

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

    "drop the test database" in {
      ReactiveMongoPlugin.db.drop()(ExecutionContext.Implicits.global)
      1 === 1
    }

    step(play.api.Play.stop())
  }

}