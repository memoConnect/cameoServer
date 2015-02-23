import models.Conversation
import play.api.libs.json._
import play.api.libs.json.JsObject
import play.api.test._
import play.api.test.FakeApplication
import play.api.test.Helpers._
import scala.Some
import scala.Some
import testHelper.Helper._
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import play.api.Logger
import testHelper.{ StartedApp, Helper }
import org.specs2.mutable._
import testHelper.TestConfig._

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

/**
 * User: Björn Reimer
 * Date: 3/3/14
 * Time: 4:48 PM
 */
class ConversationControllerSpec extends StartedApp {

  sequential

  var cidNew = ""
  val newSubject = "moep"
  val validRecipients = Seq(Json.obj("identityId" -> "kJIcR9bXwZ1os5ckqTcn"), Json.obj("identityId" -> "LhqsHt6VFtgBHGC6u4A0"))
  val validRecipientsWithSender = Seq(Json.obj("identityId" -> "kJIcR9bXwZ1os5ckqTcn"), Json.obj("identityId" -> "LhqsHt6VFtgBHGC6u4A0"), Json.obj("identityId" -> identityExisting))
  val validRecipientsWithKeys:Seq[JsObject] =
    Seq(
      Json.obj(
        "identityId" -> "kJIcR9bXwZ1os5ckqTcn",
        "keys" -> Seq(
          Json.obj("id" -> "lalskdkasljdf"),
          Json.obj("id" -> "lalskdkasdfdf"))
      ), Json.obj(
        "identityId" -> "LhqsHt6VFtgBHGC6u4A0",
        "keys" -> Seq(
          Json.obj("id" -> "lddsdfsd"))
      ), Json.obj(
        "identityId" -> identityExisting,
        "keys" -> Seq(
          Json.obj("id" -> "lddsdfsdasdasd"))
      ))
  val validRecipientsString = Seq("kJIcR9bXwZ1os5ckqTcn", "LhqsHt6VFtgBHGC6u4A0")
  val recipientMemberOfConversation = "Tya0cZiaYFhFOBS2RNP1"
  val encryptedKey = "foobarbaz!"
  val passCaptchaId = "NOBKao9AhhXUaBZVNevr"
  var numberOfConversations = 0
  val conversationSignatures= Seq(Json.obj("keyId" -> "moepSigMoepSigMoepSig", "content" -> ""))

  "ConversationController" should {

    "Get an existing conversation with messages" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      val r: JsObject = (data \ "recipients")(0).as[JsObject]
      (r \ "identityId").asOpt[String] must beSome(identityExisting)
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "numberOfMessages").asOpt[Int] must beSome(100)
      (data \ "created").asOpt[Long] must beSome
      (data \ "lastUpdated").asOpt[Long] must beSome
      (data \ "unreadMessages").asOpt[Int] must beSome(0)
    }

    "Get only messages an existing conversation with messages" in {
      val path = basePath + "/conversation/" + cidExisting + "/messages"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beNone
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "numberOfMessages").asOpt[Int] must beSome(100)
      (data \ "created").asOpt[Long] must beNone
      (data \ "lastUpdated").asOpt[Long] must beNone
      (data \ "sePassphrase").asOpt[String] must beNone
      (data \ "passCaptcha").asOpt[String] must beNone
      (data \ "aePassphraseList").asOpt[List[JsObject]] must beNone
      (data \ "keyTransmission").asOpt[String] must beNone

    }

    val keyTransmission = "moepSecure"
    "Create a new conversation with subject" in {
      val path = basePath + "/conversation"

      val subject = "test subject"
      val json = Json.obj("subject" -> subject, "keyTransmission" -> keyTransmission)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      val recipient = (data \ "recipients")(0).as[JsObject]
      (recipient \ "identityId").asOpt[String] must beSome(identityExisting)
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[Long] must beSome
      (data \ "lastUpdated").asOpt[Long] must beSome
      (data \ "subject").asOpt[String] must beSome(subject)
      (data \ "keyTransmission").asOpt[String] must beSome(keyTransmission)
      (data \ "unreadMessages").asOpt[Int] must beSome(0)
    }

    "Create a new conversation without subject" in {
      val path = basePath + "/conversation"

      val req = FakeRequest(POST, path).withJsonBody(Json.obj()).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      cidNew = (data \ "id").as[String]
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      val recipient = (data \ "recipients")(0).as[JsObject]
      (recipient \ "identityId").asOpt[String] must beSome(identityExisting)
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[Long] must beSome
      (data \ "lastUpdated").asOpt[Long] must beSome
      (data \ "subject").asOpt[String] must beNone
    }

    "Get the created conversation" in {
      val path = basePath + "/conversation/" + cidNew

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      val r: JsObject = (data \ "recipients")(0).as[JsObject]
      (r \ "identityId").asOpt[String] must beSome(identityExisting)
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[Long] must beSome
      (data \ "lastUpdated").asOpt[Long] must beSome
      (data \ "subject").asOpt[String] must beNone
    }

    var cidNew2 = ""
    "Create new conversation with recipients as strings" in {
      val path = basePath + "/conversation"

      val json = Json.obj("recipients" -> validRecipients)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      cidNew2 = (data \ "id").as[String]
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[Long] must beSome
      (data \ "lastUpdated").asOpt[Long] must beSome
      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      (data \ "recipients").as[Seq[JsObject]] must containTheSameElementsAs(validRecipientsWithSender)
    }

    "Get the created conversation" in {
      val path = basePath + "/conversation/" + cidNew2

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[Long] must beSome
      (data \ "lastUpdated").asOpt[Long] must beSome
      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      (data \ "recipients").as[Seq[JsObject]] must containTheSameElementsAs(validRecipientsWithSender)

    }

    "Create new conversation with recipients as objects" in {
      val path = basePath + "/conversation"

      val json = Json.obj("recipients" -> validRecipients)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      cidNew2 = (data \ "id").as[String]
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[Long] must beSome
      (data \ "lastUpdated").asOpt[Long] must beSome
      (data \ "conversationSignatures").asOpt[String] must beNone
      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      (data \ "recipients").as[Seq[JsObject]] must containTheSameElementsAs(validRecipientsWithSender)

    }

    "Get the created conversation" in {
      val path = basePath + "/conversation/" + cidNew2

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[Long] must beSome
      (data \ "lastUpdated").asOpt[Long] must beSome
      (data \ "conversationSignatures").asOpt[String] must beNone
      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      (data \ "recipients").as[Seq[JsObject]] must containTheSameElementsAs(validRecipientsWithSender)
    }

    "Create new conversation with recipients and keys" in {
      val path = basePath + "/conversation"

      val json = Json.obj("recipients" -> validRecipientsWithKeys)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      cidNew2 = (data \ "id").as[String]
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[Long] must beSome
      (data \ "lastUpdated").asOpt[Long] must beSome
      (data \ "conversationSignatures").asOpt[String] must beNone
      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      (data \ "recipients").as[Seq[JsObject]] must containTheSameElementsAs(validRecipientsWithKeys)
    }

    "Get the created conversation" in {
      val path = basePath + "/conversation/" + cidNew2

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[Long] must beSome
      (data \ "lastUpdated").asOpt[Long] must beSome
      (data \ "conversationSignatures").asOpt[String] must beNone
      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      (data \ "recipients").as[Seq[JsObject]] must containTheSameElementsAs(validRecipientsWithKeys)
    }

    "Create new conversation with recipients and keys and conversationSignatures" in {
      val path = basePath + "/conversation"

      val json = Json.obj("recipients" -> validRecipientsWithKeys, "conversationSignatures" -> conversationSignatures)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      cidNew2 = (data \ "id").as[String]
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[Long] must beSome
      (data \ "lastUpdated").asOpt[Long] must beSome
      (data \ "conversationSignatures").asOpt[Seq[JsObject]] must beSome(conversationSignatures)
      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      (data \ "recipients").as[Seq[JsObject]] must containTheSameElementsAs(validRecipientsWithKeys)
    }

    "Get the created conversation" in {
      val path = basePath + "/conversation/" + cidNew2

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[Long] must beSome
      (data \ "lastUpdated").asOpt[Long] must beSome
      (data \ "conversationSignatures").asOpt[Seq[JsObject]] must beSome(conversationSignatures)
      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      (data \ "recipients").as[Seq[JsObject]] must containTheSameElementsAs(validRecipientsWithKeys)
    }


    "Refuse to create new conversation with recipients that are not in the address book" in {
      val path = basePath + "/conversation"

      val json = Json.obj("recipients" -> Seq(identityExisting2))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
    }

    var cidNew4 = ""
    val messageText = "moepmoepmoepmoepmoep"
    "Create a new conversation with messages" in {
      val path = basePath + "/conversation"
      val json = Json.obj("messages" -> Seq(Json.obj("plain" -> Json.obj("text" -> messageText))))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      cidNew4 = (data \ "id").as[String]

      (data \ "messages")(0).asOpt[JsObject] must beSome
      val message = (data \ "messages")(0).as[JsObject]
      (message \ "plain" \ "text").asOpt[String] must beSome(messageText)
    }

    "Get the created conversation" in {
      val path = basePath + "/conversation/" + cidNew4

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      (data \ "messages")(0).asOpt[JsObject] must beSome
      val message = (data \ "messages")(0).as[JsObject]
      (message \ "plain" \ "text").asOpt[String] must beSome(messageText)
    }

    "Get an existing conversation with messages" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      val r: JsObject = (data \ "recipients")(0).as[JsObject]
      (r \ "identityId").asOpt[String] must beSome(identityExisting)
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      val m: Seq[JsObject] = (data \ "messages").as[Seq[JsObject]]
      m.length must beEqualTo(cidExistingNumberOfMessages)
      (data \ "created").asOpt[Long] must beSome
      (data \ "lastUpdated").asOpt[Long] must beSome
    }

    "Get an existing conversation with offset" in {

      val offset = Helper.random.nextInt(cidExistingNumberOfMessages)

      val path = basePath + "/conversation/" + cidExisting + "?offset=" + offset

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      val m: Seq[JsObject] = (data \ "messages").as[Seq[JsObject]]
      m.length must beEqualTo(cidExistingNumberOfMessages - offset)
    }

    "Get an existing conversation with limit" in {

      val limit = Math.max(Helper.random.nextInt(cidExistingNumberOfMessages), 1)

      val path = basePath + "/conversation/" + cidExisting + "?limit=" + limit

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      val m: Seq[JsObject] = (data \ "messages").as[Seq[JsObject]]
      m.length must beEqualTo(limit)
    }

    "Get an existing conversation with limit and offset" in {
      val limit = Math.max(Helper.random.nextInt(cidExistingNumberOfMessages), 1)
      val offset = Helper.random.nextInt(cidExistingNumberOfMessages)

      val path = basePath + "/conversation/" + cidExisting + "?offset=" + offset + "&limit=" + limit

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      val m: Seq[JsObject] = (data \ "messages").as[Seq[JsObject]]
      m.length must beEqualTo(Math.min(limit, cidExistingNumberOfMessages - offset))
    }

    "Get an existing conversation with time limit" in {

      val timeLimit: Long = 1392301170107L

      val path = basePath + "/conversation/" + cidExisting + "?timeLimit=" + timeLimit

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      val m: Seq[JsObject] = (data \ "messages").as[Seq[JsObject]]
      m.length must beEqualTo(6)
    }

    "get conversation summary" in {
      val path = basePath + "/conversation/" + cidExisting + "/summary"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "numberOfMessages").asOpt[Int] must beSome(100)
      (data \ "lastUpdated").asOpt[Long] must beSome
      (data \ "messages")(0).asOpt[JsObject] must beSome
      (data \ "messages")(1).asOpt[JsObject] must beNone
      (data \ "subject").asOpt[String] must beSome
      (data \ "unreadMessages").asOpt[Int] must beSome(0)

      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      val recipients = (data \ "recipients").as[Seq[JsObject]]

      (recipients(0) \ "identityId").asOpt[String] must beSome

    }

    "get conversations of user" in {
      val path = basePath + "/conversations"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "conversations").asOpt[Seq[JsObject]] must beSome
      val conversations = (data \ "conversations").as[Seq[JsObject]]
      numberOfConversations = conversations.length

      (data \ "numberOfConversations").asOpt[Int] must beSome(numberOfConversations)

      // the list should consist of conversation summaries
      conversations.map {
        c =>
          (c \ "id").asOpt[String] must beSome
          (c \ "numberOfMessages").asOpt[Int] must beSome
          (c \ "lastUpdated").asOpt[Long] must beSome
          (c \ "messages").asOpt[Seq[JsObject]] must beSome
          //          (c \ "messages")(1).asOpt[JsValue] must beNone ToDO: this was disabled due to problems with migrations
          (c \ "recipients").asOpt[Seq[JsObject]] must be beSome
      }
      // check if it contains ids
      conversations.exists(c => (c \ "id").asOpt[String].equals(Some(cidNew))) must beTrue
      conversations.exists(c => (c \ "id").asOpt[String].equals(Some(cidExisting))) must beTrue
    }

    "get conversations with offset" in {

      val offset = Helper.random.nextInt(numberOfConversations)

      val path = basePath + "/conversations?offset=" + offset

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "conversations").asOpt[Seq[JsObject]] must beSome
      val conversations = (data \ "conversations").as[Seq[JsObject]]

      (data \ "numberOfConversations").asOpt[Int] must beSome(numberOfConversations)
      conversations.length must beEqualTo(numberOfConversations - offset)
    }

    "get conversations with limit" in {

      val limit = Math.max(Helper.random.nextInt(numberOfConversations), 1)

      val path = basePath + "/conversations?limit=" + limit

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "conversations").asOpt[Seq[JsObject]] must beSome
      val conversations = (data \ "conversations").as[Seq[JsObject]]

      (data \ "numberOfConversations").asOpt[Int] must beSome(numberOfConversations)
      conversations.length must beEqualTo(limit)
    }

    "get conversations with limit and offset" in {

      val offset = Helper.random.nextInt(numberOfConversations)
      val limit = Math.max(Helper.random.nextInt(numberOfConversations), 1)

      val path = basePath + "/conversations?limit=" + limit + "&offset=" + offset

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "conversations").asOpt[Seq[JsObject]] must beSome
      val conversations = (data \ "conversations").as[Seq[JsObject]]

      (data \ "numberOfConversations").asOpt[Int] must beSome(numberOfConversations)
      conversations.length must beEqualTo(Math.min(limit, numberOfConversations - offset))
    }

    //    "add recipient to conversation" in {
    //      val path = basePath + "/conversation/" + cidExisting + "/recipient"
    //
    //      val json = Json.obj("recipients" -> validRecipients)
    //
    //      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
    //      val res = route(req).get
    //
    //      if (status(res) != OK) {
    //        Logger.error("Response: " + contentAsString(res))
    //      }
    //      status(res) must equalTo(OK)
    //    }
    //
    //    "try to add duplicate recipient to conversation" in {
    //      val path = basePath + "/conversation/" + cidExisting + "/recipient"
    //
    //      val json = Json.obj("recipients" -> Seq(validRecipients(0)))
    //
    //      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
    //      val res = route(req).get
    //
    //      if (status(res) != OK) {
    //        Logger.error("Response: " + contentAsString(res))
    //      }
    //      status(res) must equalTo(OK)
    //    }
    //
    //    "refuse to add recipient that is not in own address book" in {
    //      val path = basePath + "/conversation/" + cidExisting + "/recipient"
    //
    //      val json = Json.obj("recipients" -> Seq(identityExisting2))
    //
    //      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
    //      val res = route(req).get
    //
    //      status(res) must equalTo(232)
    //    }
    //
    //    "conversation should contain new recipients" in {
    //      val path = basePath + "/conversation/" + cidExisting
    //
    //      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
    //      val res = route(req).get
    //
    //      if (status(res) != OK) {
    //        Logger.error("Response: " + contentAsString(res))
    //      }
    //      status(res) must equalTo(OK)
    //
    //      val data = (contentAsJson(res) \ "data").as[JsObject]
    //
    //      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
    //      val recipients = (data \ "recipients").as[Seq[JsObject]]
    //
    //      recipients.length must beEqualTo(4)
    //
    //      recipients.exists(r => (r \ "identityId").as[String].equals(validRecipients(0))) must beTrue
    //      recipients.exists(r => (r \ "identityId").as[String].equals(validRecipients(1))) must beTrue
    //    }
    //
    //    "refuse to add non-existing recipients" in {
    //      val path = basePath + "/conversation/" + cidExisting + "/recipient"
    //
    //      val json = Json.obj("recipients" -> Seq("asdf"))
    //
    //      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
    //      val res = route(req).get
    //
    //      status(res) must equalTo(232)
    //    }
    //
    //    "refuse non-members to add recipients to conversation" in {
    //
    //      val path = basePath + "/conversation/" + cidExistingNonMember + "/recipient"
    //
    //      val json = Json.obj("recipients" -> validRecipients)
    //
    //      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
    //      val res = route(req).get
    //
    //      status(res) must equalTo(UNAUTHORIZED)
    //
    //      contentAsString(res) must contain("\"res\":\"KO\"")
    //      contentAsString(res) must contain("identity is not a member of the conversation")
    //    }
    //
    //    "delete recipient from conversation" in {
    //
    //      val path = basePath + "/conversation/" + cidExisting + "/recipient/" + validRecipients(1)
    //
    //      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting))
    //      val res = route(req).get
    //
    //      if (status(res) != OK) {
    //        Logger.error("Response: " + contentAsString(res))
    //      }
    //      status(res) must equalTo(OK)
    //    }
    //
    //    "refuse non-members to delete recipient from conversation" in {
    //
    //      val path = basePath + "/conversation/" + cidExisting + "/recipient/" + validRecipients(1)
    //
    //      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting2))
    //      val res = route(req).get
    //
    //      status(res) must equalTo(UNAUTHORIZED)
    //    }
    //
    //    "conversation must not contain deleted recipient" in {
    //      val path = basePath + "/conversation/" + cidExisting
    //
    //      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
    //      val res = route(req).get
    //
    //      if (status(res) != OK) {
    //        Logger.error("Response: " + contentAsString(res))
    //      }
    //      status(res) must equalTo(OK)
    //
    //      val data = (contentAsJson(res) \ "data").as[JsObject]
    //
    //      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
    //      val recipients = (data \ "recipients").as[Seq[JsObject]]
    //
    //      recipients.length must beEqualTo(3)
    //
    //      recipients.exists(r => (r \ "identityId").as[String].equals(validRecipients(1))) must beFalse
    //    }

    val encryptedPassphrase: Seq[(String, String)] = Seq(
      "keyId1" -> "phrase1",
      "keyId2" -> "phrase2",
      "keyId3" -> "phrase3"
    )

    "add encrypted passphrase list to conversation" in {

      val path = basePath + "/conversation/" + cidExisting + "/aePassphrases"

      val json = Json.obj("aePassphraseList" -> encryptedPassphrase.map {
        case (k, e) => Json.obj("keyId" -> k, "encryptedPassphrase" -> e)
      }
      )

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "do not return any encrypted passphrases when no keyid is given" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "aePassphraseList").asOpt[Seq[JsObject]] must beSome
    }

    "return one encrypted passphrase when one keyid is given" in {

      val path = basePath + "/conversation/" + cidExisting + "?keyId=" + encryptedPassphrase(0)._1

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "aePassphraseList").asOpt[Seq[JsObject]] must beSome
      val encPasses = (data \ "aePassphraseList").as[Seq[JsObject]]

      encPasses.length must beEqualTo(1)

      (encPasses(0) \ "keyId").asOpt[String] must beSome(encryptedPassphrase(0)._1)
      (encPasses(0) \ "encryptedPassphrase").asOpt[String] must beSome(encryptedPassphrase(0)._2)

    }

    "return multiple encrypted passphrases when multiple keyids are given" in {

      val path = basePath + "/conversation/" + cidExisting + "?keyId=" + encryptedPassphrase(0)._1 + "&keyId=" + encryptedPassphrase(1)._1

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "aePassphraseList").asOpt[Seq[JsObject]] must beSome
      val encPasses = (data \ "aePassphraseList").as[Seq[JsObject]]

      encPasses.length must beEqualTo(2)

      encPasses.find(js =>
        (js \ "keyId").asOpt[String].equals(Some(encryptedPassphrase(0)._1)) &&
          (js \ "encryptedPassphrase").asOpt[String].equals(Some(encryptedPassphrase(0)._2))
      ) must beSome

      encPasses.find(js =>
        (js \ "keyId").asOpt[String].equals(Some(encryptedPassphrase(1)._1)) &&
          (js \ "encryptedPassphrase").asOpt[String].equals(Some(encryptedPassphrase(1)._2))
      ) must beSome

    }

    "do not return encrypted passphrases in summary if no keyids are given" in {

      val path = basePath + "/conversation/" + cidExisting + "/summary"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "aePassphraseList").asOpt[Seq[JsObject]] must beSome
      (data \ "aePassphraseList").as[Seq[JsObject]].length must beEqualTo(0)
    }

    "return the encrypted passphrases in summary if keyids are given" in {

      val path = basePath + "/conversation/" + cidExisting + "/summary" + "?keyId=" + encryptedPassphrase(0)._1 + "&keyId=" + encryptedPassphrase(1)._1

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "aePassphraseList").asOpt[Seq[JsObject]] must beSome
      val encPasses = (data \ "aePassphraseList").as[Seq[JsObject]]

      encPasses.length must beEqualTo(2)

      encPasses.find(js =>
        (js \ "keyId").asOpt[String].equals(Some(encryptedPassphrase(0)._1)) &&
          (js \ "encryptedPassphrase").asOpt[String].equals(Some(encryptedPassphrase(0)._2))
      ) must beSome

      encPasses.find(js =>
        (js \ "keyId").asOpt[String].equals(Some(encryptedPassphrase(1)._1)) &&
          (js \ "encryptedPassphrase").asOpt[String].equals(Some(encryptedPassphrase(1)._2))
      ) must beSome
    }

    val newEncryptedPassphrase = (encryptedPassphrase(0)._1, "someNewKey")
    "add another encrypted passphrase with same keyId" in {
      val path = basePath + "/conversation/" + cidExisting + "/aePassphrases"

      val json = Json.obj("aePassphraseList" -> Seq(Json.obj("keyId" -> newEncryptedPassphrase._1, "encryptedPassphrase" -> newEncryptedPassphrase._2)))

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "return only the new encrypted passphrase for that keyId" in {

      val path = basePath + "/conversation/" + cidExisting + "?keyId=" + newEncryptedPassphrase._1

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "aePassphraseList").asOpt[Seq[JsObject]] must beSome
      val encPasses = (data \ "aePassphraseList").as[Seq[JsObject]]

      encPasses.length must beEqualTo(1)

      (encPasses(0) \ "keyId").asOpt[String] must beSome(newEncryptedPassphrase._1)
      (encPasses(0) \ "encryptedPassphrase").asOpt[String] must beSome(newEncryptedPassphrase._2)

    }

    "refuse non-members to edit encrypted passphrase list" in {

      val path = basePath + "/conversation/" + cidExisting + "/aePassphrases"

      val json = Json.obj("aePassphraseList" -> encryptedPassphrase.map {
        case (k, e) => Json.obj("keyId" -> k, "encryptedPassphrase" -> (e + "moep"))
      }
      )

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "add passCaptcha to conversation" in {
      val path = basePath + "/conversation/" + cidExisting

      val json = Json.obj("passCaptcha" -> passCaptchaId)

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "refuse non-member to add passCaptcha to conversation" in {
      val path = basePath + "/conversation/" + cidExisting

      val json = Json.obj("passCaptcha" -> (passCaptchaId + "moep"))

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "passCaptcha should be returned with conversation" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "passCaptcha").asOpt[String] must beSome(passCaptchaId)
    }

    val sePassphrase = "moepmoepmeopENCRYPTED"
    "add sePassphrase to conversation" in {
      val path = basePath + "/conversation/" + cidExisting

      val json = Json.obj("sePassphrase" -> sePassphrase)

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "refuse non-member to add sePassphrase to conversation" in {
      val path = basePath + "/conversation/" + cidExisting

      val json = Json.obj("sePassphrase" -> (sePassphrase + "moep"))

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "sePassphrase should be returned with conversation" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "sePassphrase").asOpt[String] must beSome(sePassphrase)
    }

    "aePassphrase should be returned with conversation summary" in {
      val path = basePath + "/conversation/" + cidExisting + "/summary"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "sePassphrase").asOpt[String] must beSome(sePassphrase)
    }

    var pubKeyId = ""
    "add public key to identity" in {
      val path = basePath + "/publicKey"

      val json = Json.obj("name" -> "name1", "key" -> "moep", "keySize" -> 123)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      pubKeyId = (data \ "id").as[String]
      1 === 1
    }

    var cidNew3 = ""
    "create new conversation with three recipients" in {
      val path = basePath + "/conversation"

      val json = Json.obj("recipients" -> Seq(internalContactIdentityId, internalContact2IdentityId))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      cidNew3 = (data \ "id").as[String]
      1 === 1
    }

    var messageId = ""
    "send a message as first recipient" in {
      val path = basePath + "/conversation/" + cidNew3 + "/message"
      val json = Json.obj("plain" -> Json.obj("text" -> "moep"))
      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      messageId = (data \ "id").as[String]
      1 == 1
    }

    "first recipient should not have any unread messages" in {
      val path = basePath + "/conversation/" + cidNew3

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "unreadMessages").asOpt[Int] must beSome(0)
    }

    "second recipient should have one unread message" in {
      val path = basePath + "/conversation/" + cidNew3

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(internalContactToken))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "unreadMessages").asOpt[Int] must beSome(1)
    }

    var messageId2a = ""
    var messageId2b = ""
    "send two messages as second recipient" in {
      val path = basePath + "/conversation/" + cidNew3 + "/message"
      val json = Json.obj("plain" -> Json.obj("text" -> "moep"))
      val req = FakeRequest(POST, path).withHeaders(tokenHeader(internalContactToken)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      messageId2a = (data \ "id").as[String]

      val req2 = FakeRequest(POST, path).withHeaders(tokenHeader(internalContactToken)).withJsonBody(json)
      val res2 = route(req2).get

      if (status(res2) != OK) {
        Logger.error("Response: " + contentAsString(res2))
      }
      status(res2) must equalTo(OK)

      val data2 = (contentAsJson(res2) \ "data").as[JsObject]

      (data2 \ "id").asOpt[String] must beSome
      messageId2b = (data2 \ "id").as[String]
      1 == 1
    }

    "first recipient should have two unread messages" in {
      val path = basePath + "/conversation/" + cidNew3

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "unreadMessages").asOpt[Int] must beSome(2)
    }

    "second recipient should have one unread message" in {
      val path = basePath + "/conversation/" + cidNew3

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(internalContactToken))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "unreadMessages").asOpt[Int] must beSome(1)
    }

    "third recipient should have three unread messages" in {
      val path = basePath + "/conversation/" + cidNew3

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(internalContact2Token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "unreadMessages").asOpt[Int] must beSome(3)
    }

    "third recipient disable unread messages" in {
      val path = basePath + "/account"
      val json = Json.obj("userSettings" -> Json.obj("enableUnreadMessages" -> false))

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(internalContact2Token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "third recipient should get -1 for unread messages" in {
      val path = basePath + "/conversation/" + cidNew3

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(internalContact2Token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "unreadMessages").asOpt[Int] must beSome(-1)
    }

    "third recipient should not be able to mark messages as read" in {
      val path = basePath + "/conversation/" + cidNew3 + "/message/" + messageId + "/read"

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(internalContact2Token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
    }

    "third recipient renable unread messages" in {
      val path = basePath + "/account"
      val json = Json.obj("userSettings" -> Json.obj("enableUnreadMessages" -> true))

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(internalContact2Token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "third recipient mark first message read" in {
      val path = basePath + "/conversation/" + cidNew3 + "/message/" + messageId + "/read"

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(internalContact2Token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "third recipient should have two unread messages now" in {
      val path = basePath + "/conversation/" + cidNew3

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(internalContact2Token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "unreadMessages").asOpt[Int] must beSome(2)
    }

    var messageId3 = ""
    "first recipient send another message" in {
      val path = basePath + "/conversation/" + cidNew3 + "/message"
      val json = Json.obj("plain" -> Json.obj("text" -> "moep"))
      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      messageId3 = (data \ "id").as[String]
      1 == 1
    }

    "third recipient should have three unread messages" in {
      val path = basePath + "/conversation/" + cidNew3

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(internalContact2Token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "unreadMessages").asOpt[Int] must beSome(3)
    }

    "third recipient send a message" in {
      val path = basePath + "/conversation/" + cidNew3 + "/message"
      val json = Json.obj("plain" -> Json.obj("text" -> "moep"))
      val req = FakeRequest(POST, path).withHeaders(tokenHeader(internalContact2Token)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      messageId3 = (data \ "id").as[String]
      1 == 1
    }

    "third recipient should still have three unread messages" in {
      val path = basePath + "/conversation/" + cidNew3

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(internalContact2Token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "unreadMessages").asOpt[Int] must beSome(3)
    }

    "third recipient mark last message read" in {
      val path = basePath + "/conversation/" + cidNew3 + "/message/" + messageId3 + "/read"

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(internalContact2Token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "third recipient should have no unread messages" in {
      val path = basePath + "/conversation/" + cidNew3

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(internalContact2Token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "unreadMessages").asOpt[Int] must beSome(0)
    }

    "refuse to search for conversations with term that is only two chars long" in {
      val path = basePath + "/conversations/search"

      val body = Json.obj("search" -> "ab")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(body)
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)

    }

    "search for conversations using the subject" in {
      val path = basePath + "/conversations/search"

      val body = Json.obj("search" -> "91npI")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(body)
      val res = route(req).get

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "numberOfMatches").asOpt[Int] must beSome(1)

      val conversations = (data \ "conversations").as[Seq[JsObject]]

      conversations.length must beEqualTo(1)
    }

    "search for conversation using the display name of recipient" in {
      val path = basePath + "/conversations/search"

      val body = Json.obj("search" -> "mmmoooeeeppp")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(body)
      val res = route(req).get

      val data = (contentAsJson(res) \ "data").as[JsObject]

      val conversations = (data \ "conversations").as[Seq[JsObject]]

      conversations.length must beEqualTo(6)
    }

    "search for conversation using the cameoId of recipient" in {
      val path = basePath + "/conversations/search"

      val body = Json.obj("search" -> "95Jo366N")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(body)
      val res = route(req).get

      val data = (contentAsJson(res) \ "data").as[JsObject]

      val conversations = (data \ "conversations").as[Seq[JsObject]]

      conversations.length must beEqualTo(6)
    }

    "search for conversation with term that matches both subject and recipient display name" in {
      val path = basePath + "/conversations/search"

      val body = Json.obj("search" -> "91np")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(body)
      val res = route(req).get

      val data = (contentAsJson(res) \ "data").as[JsObject]

      val conversations = (data \ "conversations").as[Seq[JsObject]]

      conversations.length must beEqualTo(7)
    }

    "only find conversations that the user is a member of" in {
      val path = basePath + "/conversations/search"

      val body = Json.obj("search" -> "dPxAk")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(body)
      val res = route(req).get

      val data = (contentAsJson(res) \ "data").as[JsObject]

      val conversations = (data \ "conversations").as[Seq[JsObject]]

      conversations.length must beEqualTo(0)
    }

    "leave conversation" should {

      var cidNew4 = ""

      "create conversation with two recipients" in {
        val body = Json.obj("recipients" -> Seq(internalContactIdentityId))
        val data = getData(executeRequest(POST, "/conversation", OK, Some(tokenExisting), Some(body)))

        (data \ "id").asOpt[String] must beSome
        cidNew4 = (data \ "id").as[String]
        1 === 1
      }

      var keyId = ""
      val keyId2 = "moepIdMoep2"

      "add key for first recipient" in {
        val body = Json.obj("name" -> "foo", "key" -> "blablalbalbablalalkasdkljasdfalkasmoepdfakjasdfasiudghl", "keySize" -> 1028)
        val data = getData(executeRequest(POST, "/publicKey", OK, Some(tokenExisting), Some(body)))

        keyId = (data \ "id").as[String]
        1 === 1
      }

      "add aePassphrase for first recipient" in {
        val body = Json.obj("aePassphraseList" -> Seq(Json.obj("keyId" -> keyId, "encryptedPassphrase" -> "foomoep")))
        checkOk( executeRequest(POST, "/conversation/" + cidNew4 + "/aePassphrases", OK, Some(tokenExisting), Some(body)))
      }

      "add aePassphrase for second recipient" in {
        val body = Json.obj("aePassphraseList" -> Seq(Json.obj("keyId" -> keyId2, "encryptedPassphrase" -> "fooomoep")))
        checkOk( executeRequest(POST, "/conversation/" + cidNew4 + "/aePassphrases", OK, Some(internalContactToken), Some(body)))
      }

      "second recipient should see the conversation" in {
        val data = getData(executeRequest(GET, "/conversation/" + cidNew4 + "?keyId=" + keyId, OK, Some(internalContactToken)))

        (data \ "recipients").as[Seq[JsObject]] must haveLength(2)
        (data \ "aePassphraseList").as[Seq[JsObject]] must haveLength(1)
      }

      "first recipient leaves the conversation" in {
        checkOk(executeRequest(DELETE, "/conversation/" + cidNew4 + "/recipient", OK, Some(tokenExisting)))
      }

      "the first recipient should not be able get the conversation" in {
        checkError(executeRequest(GET, "/conversation/" + cidNew4, UNAUTHORIZED, Some(tokenExisting)))
      }

      "the second recipient should still be able get the conversation and the first recipient should be marked as inactive and his aePassphrase deleted" in {
        val data = getData(executeRequest(GET, "/conversation/" + cidNew4 + "?keyId=" + keyId, OK, Some(internalContactToken)))

        (data \ "recipients").as[Seq[JsObject]] must haveLength(1)
        (data \ "inactiveRecipients").as[Seq[JsObject]] must haveLength(1)
        (data \ "aePassphraseList").as[Seq[JsObject]] must haveLength(0)
      }

      "the aePassphrase of the second recipient should still be there" in {
        val data = getData(executeRequest(GET, "/conversation/" + cidNew4 + "?keyId=" + keyId2, OK, Some(internalContactToken)))

        (data \ "recipients").as[Seq[JsObject]] must haveLength(1)
        (data \ "inactiveRecipients").as[Seq[JsObject]] must haveLength(1)
        (data \ "aePassphraseList").as[Seq[JsObject]] must haveLength(1)
      }

      "the talk should not appear in the conversation list of the first recipient" in {
        val data = getData(executeRequest(GET, "/conversations", OK, Some(tokenExisting)))

        val conversations = (data \ "conversations").as[Seq[JsObject]]
        conversations.exists(c => (c \ "id").asOpt[String].equals(Some(cidNew4))) must beFalse
      }

      "the second recipient leaves the conversation" in {
        checkOk(executeRequest(DELETE, "/conversation/" + cidNew4 + "/recipient", OK, Some(internalContactToken)))
      }

      "the second should not see the conversation anymore" in {
        checkError(executeRequest(GET, "/conversation/" + cidNew4, NOT_FOUND, Some(internalContactToken)))
      }

      "the conversation should be deleted from the database" in {
        val res = Await.result(Conversation.find(cidNew4), FiniteDuration(1, "min"))
        res must beNone
      }
    }
  }
}