
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
import testHelper.{ StartedApp, Stuff }
import org.specs2.mutable._
import testHelper.TestConfig._

/**
 * User: Björn Reimer
 * Date: 3/3/14
 * Time: 4:48 PM
 */
class ConversationControllerSpec extends StartedApp {

  sequential

  var cidNew = ""
  val newSubject = "moep"
  val validRecipients = Seq("6iOuCefN12ma0wF7QxR5", "dKeg67XtSNBCFMq8WQor")
  val recipientMemberOfConversation = "Tya0cZiaYFhFOBS2RNP1"
  val encryptedKey = "foobarbaz!"
  var numberOfConversations = 0

  "ConversationController" should {

    "Get an existing conversation with messages" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      val r: JsObject = (data \ "recipients")(0).as[JsObject]
      (r \ "identityId").asOpt[String] must beSome(identityExisting)
      (r \ "identity" \ "displayName").asOpt[String] must beSome
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      val m: Seq[JsObject] = (data \ "messages").as[Seq[JsObject]]
      m.length must beEqualTo(100)
      (data \ "created").asOpt[String] must beSome
      (data \ "lastUpdated").asOpt[String] must beSome
    }

    "Create a new conversation with subject" in {
      val path = basePath + "/conversation"

      val subject = "test subject"
      val json = Json.obj("subject" -> subject)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
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

      val req = FakeRequest(POST, path).withJsonBody(Json.obj()).withHeaders(tokenHeader(tokenExisting))
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

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      val r: JsObject = (data \ "recipients")(0).as[JsObject]
      (r \ "identityId").asOpt[String] must beSome(identityExisting)
      (r \ "identity" \ "displayName").asOpt[String] must beSome
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      (data \ "created").asOpt[String] must beSome
      (data \ "lastUpdated").asOpt[String] must beSome
      (data \ "subject").asOpt[String] must beNone
    }

    "Get an existing conversation with messages" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "recipients")(0).asOpt[JsObject] must beSome
      val r: JsObject = (data \ "recipients")(0).as[JsObject]
      (r \ "identityId").asOpt[String] must beSome(identityExisting)
      (r \ "identity" \ "displayName").asOpt[String] must beSome
      (data \ "messages").asOpt[Seq[JsObject]] must beSome
      val m: Seq[JsObject] = (data \ "messages").as[Seq[JsObject]]
      m.length must beEqualTo(100)
      (data \ "created").asOpt[String] must beSome
      (data \ "lastUpdated").asOpt[String] must beSome
    }

    "get conversation summary" in {
      val path = basePath + "/conversation/" + cidExisting + "/summary"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "numberOfMessages").asOpt[Int] must beSome(100)
      (data \ "lastUpdated").asOpt[String] must beSome
      (data \ "messages")(0).asOpt[JsObject] must beSome
      (data \ "messages")(1).asOpt[JsObject] must beNone
      (data \ "subject").asOpt[String] must beSome

    }

    "get conversations of user" in {
      val path = basePath + "/conversations"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

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
          (c \ "lastUpdated").asOpt[String] must beSome
          (c \ "messages").asOpt[JsArray] must beSome
      }
      // check if it contains ids
      conversations.exists(c => (c \ "id").asOpt[String].equals(Some(cidNew))) must beTrue
      conversations.exists(c => (c \ "id").asOpt[String].equals(Some(cidExisting))) must beTrue
    }

    "get conversations with offset" in {

      val offset = Stuff.random.nextInt(numberOfConversations)

      val path = basePath + "/conversations?offset=" + offset

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "conversations").asOpt[Seq[JsObject]] must beSome
      val conversations = (data \ "conversations").as[Seq[JsObject]]

      (data \ "numberOfConversations").asOpt[Int] must beSome(numberOfConversations)
      conversations.length must beEqualTo(numberOfConversations - offset)
    }

    "get conversations with limit" in {

      val limit = Stuff.random.nextInt(numberOfConversations)

      val path = basePath + "/conversations?limit=" + limit

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "conversations").asOpt[Seq[JsObject]] must beSome
      val conversations = (data \ "conversations").as[Seq[JsObject]]

      (data \ "numberOfConversations").asOpt[Int] must beSome(numberOfConversations)
      conversations.length must beEqualTo(limit)
    }

    "get conversations with limit and offset" in {

      val offset = Stuff.random.nextInt(numberOfConversations)
      val limit = Stuff.random.nextInt(numberOfConversations)

      val path = basePath + "/conversations?limit=" + limit + "&offset=" + offset

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "conversations").asOpt[Seq[JsObject]] must beSome
      val conversations = (data \ "conversations").as[Seq[JsObject]]

      (data \ "numberOfConversations").asOpt[Int] must beSome(numberOfConversations)
      conversations.length must beEqualTo(Math.min(limit, numberOfConversations-offset))
    }

    "Edit subject of an conversation" in {
      val path = basePath + "/conversation/" + cidExisting

      val json = Json.obj("subject" -> newSubject)

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "Check if subject has changed" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "subject").asOpt[String] must beSome(newSubject)
    }

    "Refuse non-member to edit subject of an conversation" in {
      val path = basePath + "/conversation/" + cidExisting

      val json = Json.obj("subject" -> newSubject)

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "add recipient to conversation" in {
      val path = basePath + "/conversation/" + cidExisting + "/recipient"

      val json = Json.obj("recipients" -> validRecipients)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "refuse to add duplicate recipient to conversation" in {
      val path = basePath + "/conversation/" + cidExisting + "/recipient"

      val json = Json.obj("recipients" -> Seq(validRecipients(0)))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(232)
    }

    "conversation should contain new recipients" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
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

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)

      contentAsString(res) must contain("\"res\":\"KO\"")
      contentAsString(res) must contain("invalid")
    }

    "refuse non-members to add recipients to conversation" in {

      val path = basePath + "/conversation/" + cidOther + "/recipient"

      val json = Json.obj("recipients" -> validRecipients)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)

      contentAsString(res) must contain("\"res\":\"KO\"")
      contentAsString(res) must contain("identity is not a member of the conversation")
    }

    "delete recipient from conversation" in {

      val path = basePath + "/conversation/" + cidExisting + "/recipient/" + validRecipients(1)

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "refuse non-members to delete recipient from conversation" in {

      val path = basePath + "/conversation/" + cidExisting + "/recipient/" + validRecipients(1)

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "conversation must not contain deleted recipient" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      val recipients = (data \ "recipients").as[Seq[JsObject]]

      recipients.length must beEqualTo(3)

      recipients.exists(r => (r \ "identityId").as[String].equals(validRecipients(1))) must beFalse
    }

    "add encrypted key to recipient in conversation" in {

      val path = basePath + "/conversation/" + cidExisting + "/recipient/" + validRecipients(0)

      val json = Json.obj("encryptedKey" -> encryptedKey)

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "return the encrypted key" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      val recipients = (data \ "recipients").as[Seq[JsObject]]

      val recipient0 = recipients.find(r => (r \ "identityId").as[String].equals(validRecipients(0))).get
      (recipient0 \ "encryptedKey").asOpt[String] must beSome(encryptedKey)

    }

    "refuse non-members to add encrypted keys to recipient" in {

      val path = basePath + "/conversation/" + cidExisting + "/recipient/" + validRecipients(0)

      val json = Json.obj("encryptedKey" -> encryptedKey)

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

  }

}