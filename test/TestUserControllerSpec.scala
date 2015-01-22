import play.api.Logger
import play.api.libs.json.{ JsObject, Json }
import play.api.test.FakeRequest
import play.api.test.Helpers._
import testHelper.StartedApp
import testHelper.Helper._
import testHelper.TestConfig._

/**
 * User: Björn Reimer
 * Date: 24.06.14
 * Time: 15:36
 */
class TestUserControllerSpec extends StartedApp {

  sequential

  val testUserId = "asdfmoep"
  val loginName = testUserPrefix + "_" + testUserId
  val password = "somePassword"

  "TestUserController" should {

    var regSec = ""

    "Reserve Login" in {
      val path = basePath + "/account/check"
      val json = Json.obj("loginName" -> loginName)

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

    var testUserIdentity = ""
    "Create Test User Account" in {
      val path = basePath + "/account"
      val json = Json.obj("loginName" -> loginName) ++
        Json.obj("password" -> password) ++
        Json.obj("phoneNumber" -> "+4912345678") ++
        Json.obj("reservationSecret" -> regSec)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      val identity = (data \ "identities")(0).as[JsObject]
      testUserIdentity = (identity \ "id").as[String]

      1 === 1
    }

    var testUserToken = ""
    "Login as testUser" in {

      val path = basePath + "/token"

      val auth = "Basic " + new sun.misc.BASE64Encoder().encode((loginName + ":" + password).getBytes)

      val req = FakeRequest(GET, path).withHeaders(("Authorization", auth))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "token").asOpt[String] must beSome
      testUserToken = (data \ "token").as[String]
      1 === 1
    }

    var conversationId = ""
    val messageText= "fooBaaMoep"
    "Start Conversation with TestUser" in {

      // send friend request
      {
        val path = basePath + "/friendRequest"

        val json = Json.obj("identityId" -> identityExisting)

        val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUserToken)).withJsonBody(json)
        val res = route(req).get

        if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
      }
      //accept friend request
      {
        val path = basePath + "/friendRequest/answer"

        val json = Json.obj("answerType" -> "accept", "identityId" -> testUserIdentity)

        val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
        val res = route(req).get

        if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
      }
      // create conversation
      {
        val path = basePath + "/conversation"

        val json = Json.obj("recipients" -> Seq(identityExisting))

        val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(testUserToken))
        val res = route(req).get

        if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
        val data = (contentAsJson(res) \ "data").as[JsObject]

        (data \ "id").asOpt[String] must beSome
        conversationId = (data \ "id").as[String]
      }
      // send Message
      {
        val path = basePath + "/conversation/" + conversationId + "/message"

        val json = Json.obj("plain" -> Json.obj("text" -> messageText))

        val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
        val res = route(req).get

        if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
      }
    }

    "Get Test User Notifications" in {
      Thread.sleep(400)
      val path = basePath + "/testUser/" + testUserId

      val req = FakeRequest(GET, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(2)
      (data(0) \ "content").asOpt[String] must beSome
      (data(0) \ "messageType").asOpt[String] must beSome("sms")
    }
    
    "Delete testUser" in {

      val path = basePath + "/testUser/" + testUserId

      val req = FakeRequest(DELETE, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "Token should not work anymore" in {
      Thread.sleep(300)
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(testUserToken))
      val res = route(req).get

      if (status(res) != UNAUTHORIZED) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(UNAUTHORIZED)
    }

    "TestUser should not be able to login" in {

      val path = basePath + "/token"

      val auth = "Basic " + new sun.misc.BASE64Encoder().encode((loginName + ":" + password).getBytes)

      val req = FakeRequest(GET, path).withHeaders(("Authorization", auth))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "Conversation should be gone" in {

      val path = basePath + "/conversation/" + conversationId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(NOT_FOUND)
    }

    "Test User Notifications should be gone" in {
      val path = basePath + "/testUser/" + testUserId

      val req = FakeRequest(GET, path)
      val res = route(req).get

      status(res) must equalTo(NOT_FOUND)
    }
  }
}
