import actors.NewMessage
import org.specs2.matcher.{MatchResult, Matcher, SomeMatcher}
import play.api.libs.json.{JsArray, Json, JsObject}
import play.api.{Logger, Play}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import scala.annotation.tailrec
import scala.concurrent.Future
import testHelper.{ TestConfig, StartedApp }
import testHelper.Stuff._
import testHelper.TestConfig._
import play.api.Play.current

/**
 * User: Björn Reimer
 * Date: 09.05.14
 * Time: 13:58
 */
class EventControllerSpec extends StartedApp {

  sequential

  var subscriptionId = ""
  var subscription2Id = ""
  var subscriptionOtherId = ""
  
  val testUser1 = createTestUser()
  val testUser2 = createTestUser()
  val testUser3 = createTestUser()

//  makeFriends(testUser1, testUser3)

  def eventNameFinder(name: String): JsObject => Boolean = {
     js => (js \ "name").as[String].equals(name)
  }

  def checkEvent[A](events: Seq[JsObject], find: JsObject => Boolean, matcher: JsObject => MatchResult[A]): MatchResult[A] = {
    events.find(find) must beSome
    val event = events.find(find).get
    matcher(event)
  }

  def waitForEvents(token: String, subscriptionId: String, count: Int): Seq[JsObject] = {

    val path = basePath + "/eventSubscription/" + subscriptionId
    val req = FakeRequest(GET, path).withHeaders(tokenHeader(token))

    def getEvents(oldEvents: Seq[JsObject], triesLeft: Int): Seq[JsObject] = {

      triesLeft must beGreaterThan(0)

      val res = route(req).get
      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(subscriptionId)
      (data \ "events").asOpt[Seq[JsObject]] must beSome

      val events = (data \ "events").as[Seq[JsObject]] ++ oldEvents

      events.length must beLessThanOrEqualTo(count)

      events.length match {
        case l if l == count => events
        case l => events ++ getEvents(events, triesLeft -1)
      }
    }

    getEvents(Seq(), 5)
  }

  "EventController" should {

    "Get a new subscription Id" in {
      val path = basePath + "/eventSubscription"
      val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser1.token)).withJsonBody(Json.obj())

      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      subscriptionId = (data \ "id").as[String]
      (data \ "events").asOpt[Seq[JsObject]] must beSome(haveLength[Seq[JsObject]](0))
    }

    "Get events" in {
      val path = basePath + "/eventSubscription/" + subscriptionId
      val req = FakeRequest(GET, path).withHeaders(tokenHeader(testUser1.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(subscriptionId)
      (data \ "events").asOpt[Seq[JsObject]] must beSome(haveLength[Seq[JsObject]](0))
    }

    "Only allow limited amount per user" in {
      val max = Play.configuration.getInt("events.subscription.user.limit").get + 1
      val path = basePath + "/eventSubscription"
      val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser2.token)).withJsonBody(Json.obj())

      (1 to max).seq.foreach { i =>
        val res = route(req).get
        i match {
          case j if j >= max => status(res) must equalTo(BAD_REQUEST)
          case _ =>
            if (status(res) != OK) {
              Logger.error("Response: " + contentAsString(res))
            }
            status(res) must equalTo(OK)
        }
      }

      1 === 1
    }

    "Get another event subscription" in {
      val path = basePath + "/eventSubscription"

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser1.token)).withJsonBody(Json.obj())
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      subscription2Id = (data \ "id").as[String]
      (data \ "events").asOpt[Seq[JsObject]] must beSome(haveLength[Seq[JsObject]](0))
    }

    "Get event subscription of other user" in {
      val path = basePath + "/eventSubscription"

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser3.token)).withJsonBody(Json.obj())
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      subscriptionOtherId = (data \ "id").as[String]
      (data \ "events").asOpt[Seq[JsObject]] must beSome(haveLength[Seq[JsObject]](0))
    }

    val friendRequestMessage = "hi_there_moep"
    "Send FriendRequest" in {
      val path = basePath + "/friendRequest"

      val json = Json.obj("identityId" -> testUser1.identityId, "message" -> friendRequestMessage)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser3.token)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "friendRequest:new event should appear in both subscriptions" in {
      val events1 = waitForEvents(testUser1.token, subscriptionId, 1)
      val events2 = waitForEvents(testUser1.token, subscription2Id, 1)

      def eventCheck(js: JsObject): MatchResult[Option[String]] = {
        (js \ "data" \ "friendRequest" \ "message").asOpt[String] must beSome(friendRequestMessage)
        (js \ "data" \ "friendRequest" \ "identity").asOpt[JsObject] must beSome
        (js \ "data" \ "to").asOpt[String] must beSome(testUser1.identityId)
      }
      checkEvent(events1, eventNameFinder("friendRequest:new"), eventCheck)
      checkEvent(events2, eventNameFinder("friendRequest:new"), eventCheck)
    }

    "Events should be cleared" in {
      waitForEvents(testUser1.token, subscriptionId, 0)
      waitForEvents(testUser1.token, subscription2Id, 0)
      1===1
    }

    "Accept friend request" in {
      val path = basePath + "/friendRequest/answer"

      val json = Json.obj("answerType" -> "accept", "identityId" -> testUser3.identityId)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser1.token)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "friendRequest:accepted event should appear in both subscriptions of first user" in {
      val events1 = waitForEvents(testUser1.token, subscriptionId, 1)
      val events2 = waitForEvents(testUser1.token, subscription2Id, 1)

      def eventCheck(js: JsObject) = {
        (js \ "data" \ "to").asOpt[String] must beSome(testUser1.identityId)
        (js \ "data" \ "from").asOpt[String] must beSome(testUser3.identityId)
        (js \ "data" \ "contact" \ "id").asOpt[String] must beSome
        (js \ "data" \ "contact" \ "identity").asOpt[JsObject] must beSome
      }

      checkEvent(events1, eventNameFinder("friendRequest:accepted"), eventCheck)
      checkEvent(events2, eventNameFinder("friendRequest:accepted"), eventCheck)
    }

    "friendRequest:accepted event should appear in subscription of second user" in {
      val events1 = waitForEvents(testUser3.token, subscriptionOtherId, 1)

      def eventCheck(js: JsObject) = {
        (js \ "data" \ "to").asOpt[String] must beSome(testUser1.identityId)
        (js \ "data" \ "from").asOpt[String] must beSome(testUser3.identityId)
        (js \ "data" \ "contact" \ "id").asOpt[String] must beSome
        (js \ "data" \ "contact" \ "identity").asOpt[JsObject] must beSome
      }

      checkEvent(events1, eventNameFinder("friendRequest:accepted"), eventCheck)
    }

    val numberOfMessages = 3
    val text = "the FooBaaMoep"
    var conversationId = ""
    "Create conversation" in {
      // create conversation
      val path = basePath + "/conversation"
      val json = Json.obj("recipients" -> Seq(testUser3.identityId))
      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(testUser1.token))
      val res = route(req).get
      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }

      status(res) must equalTo(OK)
      conversationId = (contentAsJson(res) \ "data" \ "id").as[String]

      1 === 1
    }

    "conversation:new events should appear in both subscriptions of first user" in {
      val events1 = waitForEvents(testUser1.token, subscriptionId, 1)
      val events2 = waitForEvents(testUser1.token, subscription2Id, 1)

      def eventCheck(js: JsObject) = {
        (js \ "data" \ "id").asOpt[String] must beSome(conversationId)
        (js \ "data" \ "recipients").asOpt[Seq[JsObject]] must beSome
        (js \ "data" \ "messages").asOpt[Seq[JsObject]] must beSome
      }

      checkEvent(events1, eventNameFinder("conversation:new"), eventCheck)
      checkEvent(events2, eventNameFinder("conversation:new"), eventCheck)
    }

    "Events should be cleared" in {
      waitForEvents(testUser1.token, subscriptionId, 0)
      waitForEvents(testUser1.token, subscription2Id, 0)
      1===1
    }

    "conversation:new events should appear in subscription of second user" in {
      val events1 = waitForEvents(testUser3.token, subscriptionOtherId, 1)

      def eventCheck(js: JsObject) = {
        (js \ "data" \ "id").asOpt[String] must beSome(conversationId)
        (js \ "data" \ "recipients").asOpt[Seq[JsObject]] must beSome
        (js \ "data" \ "messages").asOpt[Seq[JsObject]] must beSome
      }

      checkEvent(events1, eventNameFinder("conversation:new"), eventCheck)
    }

    "Send some messages" in {
      // send messages
      (1 to numberOfMessages).map { i =>
        val path2 = basePath + "/conversation/" + conversationId + "/message"
        val json2 = Json.obj("plain" -> Json.obj("text" -> ( text + i)))
        val req2 = FakeRequest(POST, path2).withHeaders(tokenHeader(testUser1.token)).withJsonBody(json2)
        val res2 = route(req2).get
        status(res2) must equalTo(OK)
      }
      1 === 1
    }

    "new-message events should appear in both subscriptions of first user" in {

      val events1 = waitForEvents(testUser1.token, subscriptionId, numberOfMessages)
      val events2 = waitForEvents(testUser1.token, subscription2Id, numberOfMessages)

      def eventCheck(js: JsObject) = (js \ "data" \ "conversationId").asOpt[String] must beSome(conversationId)

      (1 to numberOfMessages).map { i =>
        def finder(js: JsObject) = {
          (js \ "data" \ "message" \ "plain" \ "text").as[String].equals(text + i) &&
          eventNameFinder("conversation:new-message")(js)
        }

        checkEvent(events1, finder, eventCheck)
        checkEvent(events2, finder, eventCheck)
      }
      1 === 1
    }

    "Events should be cleared" in {
      waitForEvents(testUser1.token, subscriptionId, 0)
      waitForEvents(testUser1.token, subscription2Id, 0)
      1===1
    }

    "new-message events should appear in subscription of second user" in {

      val events1 = waitForEvents(testUser3.token, subscriptionOtherId, numberOfMessages)

      def eventCheck(js: JsObject) = (js \ "data" \ "conversationId").asOpt[String] must beSome(conversationId)

      (1 to numberOfMessages).map { i =>
        def finder(js: JsObject) = {
          (js \ "data" \ "message" \ "plain" \ "text").as[String].equals(text + i) &&
            eventNameFinder("conversation:new-message")(js)
        }

        checkEvent(events1, finder, eventCheck)
      }
      1 === 1
    }

    val fileName: String = "moepFile"
    var fileId = ""
    "upload file" in {
      val path = basePath + "/file"

      val header: Seq[(String, String)] = Seq(
        ("X-File-Name", fileName),
        ("X-Max-Chunks", "1"),
        ("X-File-Size", "123"),
        ("X-File-Type", "moep")) :+
        tokenHeader(tokenExisting)

      val req = FakeRequest(POST, path).withHeaders(header: _*)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      fileId = (data \ "id").as[String]
      1 === 1
    }

    var messageId = ""
    "send Message with file" in {
      val path = basePath + "/conversation/" + conversationId + "/message"
      val json = Json.obj("plain" -> Json.obj("text" -> text, "fileIds" -> Seq(fileId)))
      val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser1.token)).withJsonBody(json)
      val res = route(req).get
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      messageId = (data \ "id").as[String]
      1 === 1
    }

    "clear new-message events in both subscriptions of first user" in {
      waitForEvents(testUser1.token, subscriptionId, 1)
      waitForEvents(testUser1.token, subscription2Id, 1)
      1===1
    }

    "clear new-message events in subscription of second user" in {
      waitForEvents(testUser3.token, subscriptionOtherId, 1)
      1===1
    }

    "mark file upload as complete" in {
      val path = basePath + "/file/" + fileId + "/completed"

      val json = Json.obj("messageId" -> messageId)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser1.token)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "new-message events should appear in both subscriptions of first user" in {
      val events1 = waitForEvents(testUser1.token, subscriptionId, 1)
      val events2 = waitForEvents(testUser1.token, subscription2Id, 1)

      def eventCheck(js: JsObject) = {
        (js \ "data" \ "conversationId").asOpt[String] must beSome(conversationId)
        (js \ "data" \ "message").asOpt[JsObject] must beSome
        (js \ "data" \ "message" \ "plain" \ "fileIds").asOpt[Seq[String]] must beSome
      }

      checkEvent(events1, eventNameFinder("conversation:new-message"), eventCheck)
      checkEvent(events2, eventNameFinder("conversation:new-message"), eventCheck)
    }

    "new-message events should appear in subscription of second user" in {
      val events1 = waitForEvents(testUser3.token, subscriptionOtherId, 1)

      def eventCheck(js: JsObject) = {
        (js \ "data" \ "conversationId").asOpt[String] must beSome(conversationId)
        (js \ "data" \ "message").asOpt[JsObject] must beSome
        (js \ "data" \ "message" \ "plain" \ "fileIds").asOpt[Seq[String]] must beSome
      }

      checkEvent(events1, eventNameFinder("conversation:new-message"), eventCheck)
    }

    var pubKeyId = ""
    "add public key" in {
      val path = basePath + "/identity/publicKey"

      val json = Json.obj("name" -> "moep", "key" -> "asdfasdf", "keySize" -> 123)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser1.token)).withJsonBody(json)
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

    "identity:update event should appear in both subscriptions of first user" in {
      val events1 = waitForEvents(testUser1.token, subscriptionId, 1)
      val events2 = waitForEvents(testUser1.token, subscription2Id, 1)

      def eventCheck(js: JsObject) = {
        (js \ "data" \ "id").asOpt[String] must beSome(testUser1.identityId)
        (js \ "data" \ "publicKeys").asOpt[Seq[JsObject]] must beSome
        val keys = (js \ "data" \ "publicKeys").as[Seq[JsObject]]
        (keys(0) \ "id").asOpt[String] must beSome(pubKeyId)
      }

      checkEvent(events1, eventNameFinder("identity:update"), eventCheck)
      checkEvent(events2, eventNameFinder("identity:update"), eventCheck)
    }

    "identity:update event should appear in subscription of second user" in {
      val events1 = waitForEvents(testUser3.token, subscriptionOtherId, 1)

      def eventCheck(js: JsObject) = {
        (js \ "data" \ "id").asOpt[String] must beSome(testUser1.identityId)
        (js \ "data" \ "publicKeys").asOpt[Seq[JsObject]] must beSome
        val keys = (js \ "data" \ "publicKeys").as[Seq[JsObject]]
        (keys(0) \ "id").asOpt[String] must beSome(pubKeyId)
      }

      checkEvent(events1, eventNameFinder("identity:update"), eventCheck)
    }

    "add new aePassphrases to conversation" in {
      val path = basePath + "/identity/publicKey/" + pubKeyId + "/aePassphrases"
      val json = JsArray(Seq(Json.obj("conversationId" -> conversationId, "aePassphrase" -> "huuuup")))

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser1.token)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "new-aePassphrase event should appear in both subscriptions of first user" in {
      val events1 = waitForEvents(testUser1.token, subscriptionId, 1)
      val events2 = waitForEvents(testUser1.token, subscription2Id, 1)

      def eventCheck(js: JsObject) = {
        (js \ "data" \ "conversationIds").asOpt[Seq[String]] must beSome(contain(exactly(conversationId)))
        (js \ "data" \ "keyId").asOpt[String] must beSome(pubKeyId)
      }

      checkEvent(events1, eventNameFinder("conversation:new-aePassphrase"), eventCheck)
      checkEvent(events2, eventNameFinder("conversation:new-aePassphrase"), eventCheck)
    }

    "delete public key" in {
      val path = basePath + "/identity/publicKey/" + pubKeyId

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(testUser1.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "identity:update event should appear in both subscriptions of first user" in {
      val events1 = waitForEvents(testUser1.token, subscriptionId, 1)
      val events2 = waitForEvents(testUser1.token, subscription2Id, 1)

      def eventCheck(js: JsObject) = {
        (js \ "data" \ "id").asOpt[String] must beSome(testUser1.identityId)
        (js \ "data" \ "publicKeys").asOpt[Seq[JsObject]] must beSome
        val keys = (js \ "data" \ "publicKeys").as[Seq[JsObject]]
        (keys(0) \ "id").asOpt[String] must beSome(pubKeyId)
        (keys(0) \ "deleted").asOpt[Boolean] must beSome(true)
      }

      checkEvent(events1, eventNameFinder("identity:update"), eventCheck)
      checkEvent(events2, eventNameFinder("identity:update"), eventCheck)
    }

    val eventName = "moepsEvent"
    val eventData = Json.obj("foo" -> "baa", "moep" -> "moeps")
    "send broadcast event" in {
      val path = basePath + "/event/broadcast"

      val json = Json.obj("name" -> eventName, "data" -> eventData)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser1.token)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "broadcast event should appear in both subscriptions of first user" in {
      val events1 = waitForEvents(testUser1.token, subscriptionId, 1)
      val events2 = waitForEvents(testUser1.token, subscription2Id, 1)

      def eventCheck(js: JsObject) = (js \ "data").asOpt[JsObject] must beSome(eventData)

      checkEvent(events1, eventNameFinder(eventName), eventCheck)
      checkEvent(events2, eventNameFinder(eventName), eventCheck)
    }

    val cameoId = testUserPrefix + "_" + randomString(6)
    "create new identity for first user" in {
      val path = basePath + "/identity"
      val json = Json.obj("cameoId" -> cameoId, "reservationSecret" -> "foo")

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(testUser1.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "identity:new event should appear in both subscriptions of first user" in {
      val events1 = waitForEvents(testUser1.token, subscriptionId, 1)
      val events2 = waitForEvents(testUser1.token, subscription2Id, 1)

      def eventCheck(js: JsObject) = {
        (js \ "data" \ "id").asOpt[String] must beSome
        (js \ "data" \ "cameoId").asOpt[String] must beSome(cameoId + "@" + domain)
        (js \ "data" \ "publicKeys").asOpt[Seq[JsObject]] must beSome
        (js \ "data" \ "userKey").asOpt[String] must beSome
      }

      checkEvent(events1, eventNameFinder("identity:new"), eventCheck)
      checkEvent(events2, eventNameFinder("identity:new"), eventCheck)

    }
  }
}
