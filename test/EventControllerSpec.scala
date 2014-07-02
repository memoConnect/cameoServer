import actors.NewMessage
import play.api.libs.json.{ Json, JsObject }
import play.api.{ Logger, Play }
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

  "EventController" should {

    "Get a new subscription Id" in {
      val path = basePath + "/eventSubscription"
      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(Json.obj())

      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      subscriptionId = (data \ "id").as[String]
      (data \ "events").asOpt[Seq[JsObject]] must beSome(haveLength[Seq[JsObject]](0))
    }

    "Get events" in {
      val path = basePath + "/eventSubscription/" + subscriptionId
      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(subscriptionId)
      (data \ "events").asOpt[Seq[JsObject]] must beSome(haveLength[Seq[JsObject]](0))
    }

    "Only allow limited amount per user" in {
      val max = Play.configuration.getInt("events.subscription.user.limit").get + 1
      val path = basePath + "/eventSubscription"
      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(Json.obj())

      (1 to max).seq.foreach { i =>
        val res = route(req).get
        i match {
          case j if j >= max => status(res) must equalTo(BAD_REQUEST)
          case _             => status(res) must equalTo(OK)
        }
      }

      1 === 1
    }

    "Get another event subscription" in {
      val path = basePath + "/eventSubscription"

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(Json.obj())
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      subscription2Id = (data \ "id").as[String]
      (data \ "events").asOpt[Seq[JsObject]] must beSome(haveLength[Seq[JsObject]](0))
    }

    "Get event subscription of other user" in {
      val path = basePath + "/eventSubscription"

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting3)).withJsonBody(Json.obj())
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      subscriptionOtherId = (data \ "id").as[String]
      (data \ "events").asOpt[Seq[JsObject]] must beSome(haveLength[Seq[JsObject]](0))
    }

    val friendRequestMessage = "hi_there_moep"
    "Send FriendRequest" in {
      val path = basePath + "/friendRequest"

      val json = Json.obj("identityId" -> identityExisting, "message" -> friendRequestMessage)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting3)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "friendRequest:new event should appear in both subscriptions" in {
      Thread.sleep(200)
      Seq(subscriptionId, subscription2Id).seq.map { id =>
        val path = basePath + "/eventSubscription/" + id
        val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
        val res = route(req).get

        status(res) must equalTo(OK)

        val data = (contentAsJson(res) \ "data").as[JsObject]

        (data \ "id").asOpt[String] must beSome(id)
        (data \ "events").asOpt[Seq[JsObject]] must beSome

        val events = (data \ "events").as[Seq[JsObject]]
        val newMessageEvents = events.filter(e =>
          (e \ "name").as[String].equals("friendRequest:new") && (e \ "data" \ "friendRequest" \ "identityId").asOpt[String].getOrElse("foo").equals(identityExisting3))
        newMessageEvents.length must equalTo(1)
        newMessageEvents.map { js =>
          (js \ "data" \ "friendRequest" \ "message").asOpt[String] must beSome(friendRequestMessage)
          (js \ "data" \ "friendRequest" \ "identity").asOpt[JsObject] must beSome
          (js \ "data" \ "to").asOpt[String] must beSome(identityExisting)
        }
      }
      1 === 1
    }

    "Events should be cleared" in {
      Seq(subscriptionId, subscription2Id).seq.map { id =>
        val path = basePath + "/eventSubscription/" + id
        val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
        val res = route(req).get

        status(res) must equalTo(OK)

        val data = (contentAsJson(res) \ "data").as[JsObject]
        (data \ "id").asOpt[String] must beSome(id)
        (data \ "events").asOpt[Seq[JsObject]] must beSome(haveLength[Seq[JsObject]](0))
      }
    }

    "Accept friend request" in {
      val path = basePath + "/friendRequest/answer"

      val json = Json.obj("answerType" -> "accept", "identityId" -> identityExisting3)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "friendRequest:accepted event should appear in both subscriptions of first user" in {
      Seq(subscriptionId, subscription2Id).seq.map { id =>
        val path = basePath + "/eventSubscription/" + id
        val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
        val res = route(req).get

        status(res) must equalTo(OK)

        val data = (contentAsJson(res) \ "data").as[JsObject]

        (data \ "id").asOpt[String] must beSome(id)
        (data \ "events").asOpt[Seq[JsObject]] must beSome

        val events = (data \ "events").as[Seq[JsObject]]
        val newMessageEvents = events.filter(e =>
          (e \ "name").as[String].equals("friendRequest:accepted") && (e \ "data" \ "to").asOpt[String].getOrElse("foo").equals(identityExisting))
        newMessageEvents.length must equalTo(1)
        newMessageEvents.map { js =>
          (js \ "data" \ "to").asOpt[String] must beSome(identityExisting)
          (js \ "data" \ "from").asOpt[String] must beSome(identityExisting3)
        }
      }
      1 === 1
    }

    "friendRequest:accepted event should appear in subscription of second user" in {

      val path = basePath + "/eventSubscription/" + subscriptionOtherId
      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(subscriptionOtherId)
      (data \ "events").asOpt[Seq[JsObject]] must beSome

      val events = (data \ "events").as[Seq[JsObject]]
      val newMessageEvents = events.filter(e =>
        (e \ "name").as[String].equals("friendRequest:accepted") && (e \ "data" \ "to").asOpt[String].getOrElse("foo").equals(identityExisting))
      newMessageEvents.length must equalTo(1)
      newMessageEvents.map { js =>
        (js \ "data" \ "to").asOpt[String] must beSome(identityExisting)
        (js \ "data" \ "from").asOpt[String] must beSome(identityExisting3)
      }

      1 === 1
    }

    val numberOfMessages = 3
    val text = "the FooBaaMoep"
    var conversationId = ""
    "Create conversation" in {

      // create conversation
      val path = basePath + "/conversation"
      val json = Json.obj("recipients" -> Seq(identityExisting3))
      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get
      status(res) must equalTo(OK)
      conversationId = (contentAsJson(res) \ "data" \ "id").as[String]

      1 === 1
    }

    "conversation:new events should appear in both subscriptions of first user" in {
      Thread.sleep(200)

      Seq(subscriptionId, subscription2Id).seq.map { id =>

        val path = basePath + "/eventSubscription/" + id
        val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
        val res = route(req).get

        status(res) must equalTo(OK)

        val data = (contentAsJson(res) \ "data").as[JsObject]

        (data \ "id").asOpt[String] must beSome(id)
        (data \ "events").asOpt[Seq[JsObject]] must beSome

        val events = (data \ "events").as[Seq[JsObject]]

        val newConverstionEvent = events.filter(e =>
          (e \ "name").as[String].equals("conversation:new") && (e \ "data" \ "id").asOpt[String].getOrElse("foo").equals(conversationId))
        newConverstionEvent.length must greaterThanOrEqualTo(1)
        newConverstionEvent.map { js =>
          (js \ "data" \ "id").asOpt[String] must beSome(conversationId)
          (js \ "data" \ "recipients").asOpt[Seq[JsObject]] must beSome
          (js \ "data" \ "messages").asOpt[Seq[JsObject]] must beSome
        }
      }
      1 === 1
    }

    "Events should be cleared" in {
      Seq(subscriptionId, subscription2Id).seq.map { id =>
        val path = basePath + "/eventSubscription/" + id
        val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
        val res = route(req).get

        status(res) must equalTo(OK)

        val data = (contentAsJson(res) \ "data").as[JsObject]
        (data \ "id").asOpt[String] must beSome(id)
        (data \ "events").asOpt[Seq[JsObject]] must beSome(haveLength[Seq[JsObject]](0))
      }
    }

    "conversation:new events should appear subscriptions of second user" in {
      val path = basePath + "/eventSubscription/" + subscriptionOtherId
      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(subscriptionOtherId)
      (data \ "events").asOpt[Seq[JsObject]] must beSome

      val events = (data \ "events").as[Seq[JsObject]]

      val newConverstionEvent = events.filter(e =>
        (e \ "name").as[String].equals("conversation:new") && (e \ "data" \ "id").asOpt[String].getOrElse("foo").equals(conversationId))
      newConverstionEvent.length must greaterThanOrEqualTo(1)
      newConverstionEvent.map { js =>
        (js \ "data" \ "id").asOpt[String] must beSome(conversationId)
        (js \ "data" \ "recipients").asOpt[Seq[JsObject]] must beSome
        (js \ "data" \ "messages").asOpt[Seq[JsObject]] must beSome
      }
      1 === 1
    }

    "Send some messages" in {
      // send messages
      (1 to numberOfMessages).map { i =>
        val path2 = basePath + "/conversation/" + conversationId + "/message"
        val json2 = Json.obj("plain" -> Json.obj("text" -> text))
        val req2 = FakeRequest(POST, path2).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json2)
        val res2 = route(req2).get
        status(res2) must equalTo(OK)
      }
      1 === 1
    }

    "new-message events should appear in both subscriptions of first user" in {
      Thread.sleep(200)

      Seq(subscriptionId, subscription2Id).seq.map { id =>
        val path = basePath + "/eventSubscription/" + id
        val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
        val res = route(req).get

        status(res) must equalTo(OK)

        val data = (contentAsJson(res) \ "data").as[JsObject]

        (data \ "id").asOpt[String] must beSome(id)
        (data \ "events").asOpt[Seq[JsObject]] must beSome

        val events = (data \ "events").as[Seq[JsObject]]

        val newMessageEvents = events.filter(e =>
          (e \ "name").as[String].equals("conversation:new-message") &&
            (e \ "data" \ "conversationId").asOpt[String].getOrElse("foo").equals(conversationId))
        newMessageEvents.length must greaterThanOrEqualTo(numberOfMessages)
        newMessageEvents.map { js =>
          (js \ "data" \ "conversationId").asOpt[String] must beSome(conversationId)
          (js \ "data" \ "message").asOpt[JsObject] must beSome
          (js \ "data" \ "message" \ "plain" \ "text").asOpt[String] must beSome(text)
        }
      }
      1 === 1
    }

    "Events should be cleared" in {
      Seq(subscriptionId, subscription2Id).seq.map { id =>
        val path = basePath + "/eventSubscription/" + id
        val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
        val res = route(req).get

        status(res) must equalTo(OK)

        val data = (contentAsJson(res) \ "data").as[JsObject]
        (data \ "id").asOpt[String] must beSome(id)
        (data \ "events").asOpt[Seq[JsObject]] must beSome(haveLength[Seq[JsObject]](0))
      }
    }

    "new-message events should appear in subscription of second user" in {
      val path = basePath + "/eventSubscription/" + subscriptionOtherId
      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(subscriptionOtherId)
      (data \ "events").asOpt[Seq[JsObject]] must beSome

      val events = (data \ "events").as[Seq[JsObject]]

      val newMessageEvents = events.filter(e =>
        (e \ "name").as[String].equals("conversation:new-message") &&
          (e \ "data" \ "conversationId").asOpt[String].getOrElse("foo").equals(conversationId))
      newMessageEvents.length must greaterThanOrEqualTo(numberOfMessages)
      newMessageEvents.map { js =>
        (js \ "data" \ "conversationId").asOpt[String] must beSome(conversationId)
        (js \ "data" \ "message").asOpt[JsObject] must beSome
        (js \ "data" \ "message" \ "plain" \ "text").asOpt[String] must beSome(text)
      }
      1 === 1
    }

    var pubKeyId = ""
    "add public key" in {

      val path = basePath + "/identity/publicKey"

      val json = Json.obj("name" -> "moep", "key" -> "asdfasdf", "keySize" -> 123)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      pubKeyId = (data \ "id").as[String]

      1 === 1
    }

    "identity:update event should appear in both subscriptions of first user" in {
      Thread.sleep(200)

      Seq(subscriptionId, subscription2Id).seq.map { id =>
        val path = basePath + "/eventSubscription/" + id
        val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
        val res = route(req).get

        status(res) must equalTo(OK)

        val data = (contentAsJson(res) \ "data").as[JsObject]

        (data \ "id").asOpt[String] must beSome(id)
        (data \ "events").asOpt[Seq[JsObject]] must beSome

        val events = (data \ "events").as[Seq[JsObject]]

        val newMessageEvents = events.filter(e =>
          (e \ "name").as[String].equals("identity:update"))
        newMessageEvents.length must beEqualTo(1)
        newMessageEvents.map { js =>
          (js \ "data" \ "id").asOpt[String] must beSome(identityExisting)
          (js \ "data" \ "publicKeys").asOpt[Seq[JsObject]] must beSome
          val keys = (js \ "data" \ "publicKeys").as[Seq[JsObject]]
          (keys(0) \ "id").asOpt[String] must beSome(pubKeyId)
        }
      }
      1 === 1
    }

    "identity:update event should appear in subscription of second user" in {
      val path = basePath + "/eventSubscription/" + subscriptionOtherId
      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(subscriptionOtherId)
      (data \ "events").asOpt[Seq[JsObject]] must beSome

      val events = (data \ "events").as[Seq[JsObject]]

      val newMessageEvents = events.filter(e =>
        (e \ "name").as[String].equals("identity:update"))
      newMessageEvents.length must beEqualTo(1)
      newMessageEvents.map { js =>
        (js \ "data" \ "id").asOpt[String] must beSome(identityExisting)
        (js \ "data" \ "publicKeys").asOpt[Seq[JsObject]] must beSome
        val keys = (js \ "data" \ "publicKeys").as[Seq[JsObject]]

        (keys(0) \ "id").asOpt[String] must beSome(pubKeyId)
      }
    }
  }
}
