import play.api.libs.json._
import play.api.libs.json.JsObject
import play.api.test._
import play.api.test.FakeApplication
import play.api.test.Helpers._
import scala.Some
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
  val validRecipients = Seq("kJIcR9bXwZ1os5ckqTcn", "LhqsHt6VFtgBHGC6u4A0")
  val recipientMemberOfConversation = "Tya0cZiaYFhFOBS2RNP1"
  val encryptedKey = "foobarbaz!"
  val passCaptchaId = "NOBKao9AhhXUaBZVNevr"
  var numberOfConversations = 0

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
      val m: Seq[JsObject] = (data \ "messages").as[Seq[JsObject]]
      m.length must beEqualTo(100)
      (data \ "created").asOpt[Long] must beSome
      (data \ "lastUpdated").asOpt[Long] must beSome
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
    "Create new conversation with recipients" in {
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
      (data \ "recipients").as[Seq[JsObject]].length must beEqualTo(3)
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
      (data \ "recipients").as[Seq[JsObject]].length must beEqualTo(3)
    }

    "Refuse to create new conversation with recipients that are not in the address book" in {
      val path = basePath + "/conversation"

      val json = Json.obj("recipients" -> Seq(identityExisting2))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != 232) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(232)
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
      m.length must beEqualTo(100)
      (data \ "created").asOpt[Long] must beSome
      (data \ "lastUpdated").asOpt[Long] must beSome
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

      val offset = Stuff.random.nextInt(numberOfConversations)

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

      val limit = Math.max(Stuff.random.nextInt(numberOfConversations), 1)

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

      val offset = Stuff.random.nextInt(numberOfConversations)
      val limit = Math.max(Stuff.random.nextInt(numberOfConversations), 1)

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

    "Edit subject of an conversation" in {
      val path = basePath + "/conversation/" + cidExisting

      val json = Json.obj("subject" -> newSubject)

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "Check if subject has changed" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "subject").asOpt[String] must beSome(newSubject)
    }

    val newKeyTransmission = "veryMoepSecure"
    "Edit keyTransmission of an conversation" in {
      val path = basePath + "/conversation/" + cidExisting

      val json = Json.obj("keyTransmission" -> newKeyTransmission)

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "Check if keyTransmission has changed" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "keyTransmission").asOpt[String] must beSome(newKeyTransmission)
    }

    "Refuse non-member to edit subject of an conversation" in {
      val path = basePath + "/conversation/" + cidExisting

      val json = Json.obj("subject" -> (newSubject + "moep"))

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "add recipient to conversation" in {
      val path = basePath + "/conversation/" + cidExisting + "/recipient"

      val json = Json.obj("recipients" -> validRecipients)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.debug("response: " + contentAsString(res))
      }

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "try to add duplicate recipient to conversation" in {
      val path = basePath + "/conversation/" + cidExisting + "/recipient"

      val json = Json.obj("recipients" -> Seq(validRecipients(0)))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "refuse to add recipient that is not in own address book" in {
      val path = basePath + "/conversation/" + cidExisting + "/recipient"

      val json = Json.obj("recipients" -> Seq(identityExisting2))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(232)
    }

    "conversation should contain new recipients" in {
      val path = basePath + "/conversation/" + cidExisting

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
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

      status(res) must equalTo(232)
    }

    "refuse non-members to add recipients to conversation" in {

      val path = basePath + "/conversation/" + cidExistingNonMember + "/recipient"

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

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
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

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "recipients").asOpt[Seq[JsObject]] must beSome
      val recipients = (data \ "recipients").as[Seq[JsObject]]

      recipients.length must beEqualTo(3)

      recipients.exists(r => (r \ "identityId").as[String].equals(validRecipients(1))) must beFalse
    }

    val encryptedPassphrase: Seq[(String, String)] = Seq(
      ("keyId1" -> "phrase1"),
      ("keyId2" -> "phrase2"),
      ("keyId3" -> "phrase3")
    )

    "add encrypted passphrase list to conversation" in {

      val path = basePath + "/conversation/" + cidExisting

      val json = Json.obj("aePassphraseList" -> encryptedPassphrase.map{
        case (k,e) => Json.obj("keyId" -> k, "encryptedPassphrase" -> e)}
      )

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
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

      (encPasses(0) \ "keyId").asOpt[String] must beSome(encryptedPassphrase(0)._1)
      (encPasses(0) \ "encryptedPassphrase").asOpt[String] must beSome(encryptedPassphrase(0)._2)
      (encPasses(1) \ "keyId").asOpt[String] must beSome(encryptedPassphrase(1)._1)
      (encPasses(1) \ "encryptedPassphrase").asOpt[String] must beSome(encryptedPassphrase(1)._2)
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

      (encPasses(0) \ "keyId").asOpt[String] must beSome(encryptedPassphrase(0)._1)
      (encPasses(0) \ "encryptedPassphrase").asOpt[String] must beSome(encryptedPassphrase(0)._2)
      (encPasses(1) \ "keyId").asOpt[String] must beSome(encryptedPassphrase(1)._1)
      (encPasses(1) \ "encryptedPassphrase").asOpt[String] must beSome(encryptedPassphrase(1)._2)
    }

    val newEncryptedPassphrase = (encryptedPassphrase(0)._1, "someNewKey")
    "add another encrypted passphrase with same keyId" in {
      val path = basePath + "/conversation/" + cidExisting

      val json = Json.obj("aePassphraseList" -> Seq(Json.obj("keyId" -> newEncryptedPassphrase._1, "encryptedPassphrase" -> newEncryptedPassphrase._2)))

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
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

      val path = basePath + "/conversation/" + cidExisting

      val json = Json.obj("aePassphraseList" -> encryptedPassphrase.map{
        case (k,e) => Json.obj("keyId" -> k, "encryptedPassphrase" -> (e + "moep"))}
      )

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
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
      1===1
    }

    var cidNew3 = ""
    "create new conversation with three recipients" in {
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
      cidNew3 = (data \ "id").as[String]
      1===1
    }

//    var missingPassphrasesLength = 0
//    "return missing encrypted passphrases" in {
//      val path = basePath + "/conversation/" + cidNew3
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
//      (data \ "missingAePassphrase").asOpt[Seq[String]] must beSome
//      val missingPassphrases = (data \ "missingAePassphrase").as[Seq[String]]
//
//      missingPassphrases.length must beGreaterThanOrEqualTo(3)
//      missingPassphrasesLength = missingPassphrases.length
//      missingPassphrases.find(_.equals(pubKeyId)) must beSome
//    }
//
//    "add encrypted passphrase for one key to conversation" in {
//
//      val path = basePath + "/conversation/" + cidNew3 + "/aePassphrases"
//
//      val json = Json.obj("aePassphraseList" -> Seq(Json.obj("keyId" -> pubKeyId, "encryptedPassphrase" -> "moep")))
//
//      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//    }
//
//    var missingPassphrases: Seq[String] = Seq()
//    "missing encrypted passphrases should not contain added key" in {
//      val path = basePath + "/conversation/" + cidNew3
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
//      (data \ "missingAePassphrase").asOpt[Seq[String]] must beSome
//      missingPassphrases = (data \ "missingAePassphrase").as[Seq[String]]
//
//      missingPassphrases.length must beEqualTo(missingPassphrasesLength - 1)
//      missingPassphrases.find(_.equals(pubKeyId)) must beNone
//    }
//
//    "add encrypted passphrase for remaining keys" in {
//
//      val path = basePath + "/conversation/" + cidNew3  + "/aePassphrases"
//
//      val list = missingPassphrases.map(mp => Json.obj("keyId" -> mp, "encryptedPassphrase" -> "moep"))
//
//      val json = Json.obj("aePassphraseList" -> list)
//
//      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
//      val res = route(req).get
//
//      if (status(res) != OK) {
//        Logger.error("Response: " + contentAsString(res))
//      }
//      status(res) must equalTo(OK)
//    }
//
//    "missing encrypted passphrases should now be empty" in {
//      val path = basePath + "/conversation/" + cidNew3
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
//      (data \ "missingAePassphrase").asOpt[Seq[String]] must beSome
//      val missingPassphrases = (data \ "missingAePassphrase").as[Seq[JsObject]]
//
//      missingPassphrases.length must beEqualTo(0)
//    }

  }

}