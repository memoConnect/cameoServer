import helper.TestValueStore
import play.api.Logger
import play.api.libs.json.{ JsObject, Json }
import play.api.test.FakeRequest
import play.api.test.Helpers._
import testHelper.{ Stuff, StartedApp }
import testHelper.Stuff._
import testHelper.TestConfig._

/**
 * User: Björn Reimer
 * Date: 30.07.14
 * Time: 15:26
 */
class UserNotificationSpec extends StartedApp {

  sequential

  "User Notifications" should {

    val nameExt1 = "MeopMeister"
    val nameExt2 = "MeopMaster"
    val nameExt3 = "MeopSupaMaster"
    val mailExt1 = "foo@baa.de"
    val mailExt3 = "moep@baa.de"
    val telExt2 = "+491234"
    val telExt3 = "+494561"

    var externalContactId1 = ""
    var externalContactId2 = ""
    var externalContactId3 = ""

    "create external contact with mail only" in {
      val path = basePath + "/contact"
      val json = Json.obj("identity" -> Json.obj("email" -> mailExt1, "displayName" -> nameExt1))
      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "identityId").asOpt[String] must beSome
      externalContactId1 = (data \ "identityId").as[String]
      1 === 1
    }

    "create external contact with tel only" in {
      val path = basePath + "/contact"
      val json = Json.obj("identity" -> Json.obj("phoneNumber" -> telExt2, "displayName" -> nameExt2))
      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "identityId").asOpt[String] must beSome
      externalContactId2 = (data \ "identityId").as[String]
      1 === 1
    }

    "create external contact with tel and mail" in {
      val path = basePath + "/contact"
      val json = Json.obj("identity" -> Json.obj("email" -> mailExt3, "phoneNumber" -> telExt3, "displayName" -> nameExt3))
      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "identityId").asOpt[String] must beSome
      externalContactId3 = (data \ "identityId").as[String]
      1 === 1
    }

    var conversationId = ""
    "start conversation with external and internal contacts" in {
      val path = basePath + "/conversation"

      val recipients = Seq(externalContactId1, externalContactId2, externalContactId3, identityExisting3, identityExisting4)

      val json = Json.obj("recipients" -> recipients)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      conversationId = (data \ "id").as[String]

      1 === 1
    }

    step(TestValueStore.start())

    "add message to conversation" in {
      val path = basePath + "/conversation/" + conversationId + "/message"

      val json = Json.obj("plain" -> Json.obj("text" -> "foo"))

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "all users except the sender should have received notifications" in {
      // wait until notifications have arrived
      Stuff.waitFor(TestValueStore.getValues("mail").length == 2 && TestValueStore.getValues("sms").length == 3)

      val mails = TestValueStore.getValues("mail")
      val sms = TestValueStore.getValues("sms")

      Logger.debug(mails.toString)
      Logger.debug(sms.toString)

      sms.exists(js => (js \ "to").as[String].equals(telExt2)) must beTrue
      sms.exists(js => (js \ "to").as[String].equals(telExt3)) must beTrue
      sms.exists(js => (js \ "to").as[String].equals(accountExisting2Tel)) must beFalse
      sms.exists(js => (js \ "to").as[String].equals(accountExisting3Tel)) must beTrue

      mails.exists(js => (js \ "to").as[String].equals(mailExt1)) must beTrue
      mails.exists(js => (js \ "to").as[String].equals(mailExt3)) must beFalse
      mails.exists(js => (js \ "to").as[String].equals(accountExisting2Mail)) must beFalse
      mails.exists(js => (js \ "to").as[String].equals(accountExisting4Mail)) must beTrue

      mails.length must beEqualTo(2)
      sms.length must beEqualTo(3)
    }

    var tokenExt1 = ""
    "get token for external user" in {
      // get purl
      val message = TestValueStore.getValues("mail").find(js => (js \ "to").as[String].equals(mailExt1)).get
      val purlExternal1 = (message \ "body").as[String].split("/p/")(1)

      // use purl to get token
      val path = basePath + "/purl/" + purlExternal1
      val req = FakeRequest(GET, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      tokenExt1 = (data \ "token").as[String]

      1 === 1
    }

    step(TestValueStore.stop())
    step(TestValueStore.start())

    "add event subscription to internal user" in {
      val path = basePath + "/eventSubscription"
      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting3)).withJsonBody(Json.obj())

      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "add event subscription to external user" in {
      val path = basePath + "/eventSubscription"
      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExt1)).withJsonBody(Json.obj())

      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "send another message" in {
      val path = basePath + "/conversation/" + conversationId + "/message"

      val json = Json.obj("plain" -> Json.obj("text" -> "foo"))

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "users with event subscriptions should not get notifications" in {
      // wait until notifications have arrived
      Stuff.waitFor(TestValueStore.getValues("mail").length == 1 && TestValueStore.getValues("sms").length == 2)

      val mails = TestValueStore.getValues("mail")
      val sms = TestValueStore.getValues("sms")

      sms.exists(js => (js \ "to").as[String].equals(telExt2)) must beTrue
      sms.exists(js => (js \ "to").as[String].equals(telExt3)) must beTrue
      sms.exists(js => (js \ "to").as[String].equals(accountExisting2Tel)) must beFalse
      sms.exists(js => (js \ "to").as[String].equals(accountExisting3Tel)) must beFalse

      mails.exists(js => (js \ "to").as[String].equals(mailExt1)) must beFalse
      mails.exists(js => (js \ "to").as[String].equals(mailExt3)) must beFalse
      mails.exists(js => (js \ "to").as[String].equals(accountExisting2Mail)) must beFalse
      mails.exists(js => (js \ "to").as[String].equals(accountExisting4Mail)) must beTrue

      mails.length must beEqualTo(1)
      sms.length must beEqualTo(2)
    }

    step(TestValueStore.stop())
    step(TestValueStore.start())
    "recipient send message" in {
      val path = basePath + "/conversation/" + conversationId + "/message"

      val json = Json.obj("plain" -> Json.obj("text" -> "foo"))

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting4)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

    }

    "author of conversation should now receive a notification" in {
      // wait until notifications have arrived
      Stuff.waitFor(TestValueStore.getValues("mail").length == 0 && TestValueStore.getValues("sms").length == 3)

      val mails = TestValueStore.getValues("mail")
      val sms = TestValueStore.getValues("sms")

      sms.exists(js => (js \ "to").as[String].equals(telExt2)) must beTrue
      sms.exists(js => (js \ "to").as[String].equals(telExt3)) must beTrue
      sms.exists(js => (js \ "to").as[String].equals(accountExisting2Tel)) must beTrue
      sms.exists(js => (js \ "to").as[String].equals(accountExisting3Tel)) must beFalse

      mails.exists(js => (js \ "to").as[String].equals(mailExt1)) must beFalse
      mails.exists(js => (js \ "to").as[String].equals(mailExt3)) must beFalse
      mails.exists(js => (js \ "to").as[String].equals(accountExisting2Mail)) must beFalse
      mails.exists(js => (js \ "to").as[String].equals(accountExisting4Mail)) must beFalse

      mails.length must beEqualTo(0)
      sms.length must beEqualTo(3)
    }

    step(TestValueStore.stop())
  }

}
