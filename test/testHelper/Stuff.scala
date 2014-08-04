package testHelper

import play.api.Logger
import play.api.libs.json.{ Json, JsObject }
import helper.TestValueStore
import play.api.test.FakeRequest
import play.api.test.Helpers._
import testHelper.TestConfig._

import scalaz.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 1/30/14
 * Time: 2:19 PM
 */
object Stuff {

  def toJsonOrEmpty(key: String, value: Option[String]): JsObject = {
    value.map(v => Json.obj(key -> v)).getOrElse(Json.obj())
  }

  def createUser(login: String, password: String, tel: Option[String] = None, email: Option[String] = None): JsObject = {
    Json.obj(
      "loginName" -> login,
      "password" -> password) ++
      toJsonOrEmpty("phoneNumber", tel) ++
      toJsonOrEmpty("email", email)
  }

  val random = new scala.util.Random

  def randomString(n: Int): String = {
    def alphabet: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    //TODO check whether random.setSeed is needed
    Stream.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString
  }

  def randomLengthString(max: Int) = {
    val n = random.nextInt(max)
    val nonZero = if (n == 0) 1 else n
    randomString(nonZero)
  }

  def tokenHeader(token: String): (String, String) = ("Authorization", token)

  def twoFactorTokenHeader(token: String): (String, String) = ("X-TwoFactorToken", token)

  def waitFor(test: => Boolean) {
    var maxRepetitions = 100
    while (!test && maxRepetitions > 0) {
      maxRepetitions -= 1
      Thread.sleep(50)
      if (maxRepetitions % 20 == 0) {
        Logger.debug("waiting: " + test)
      }
    }
  }

  case class TestUser(login: String, identityId: String, token: String)

  def createTestUser(tel: Option[String] = None, email: Option[String] = None): TestUser = {

    val loginName = testUserPrefix + "_" + randomString(6)
    Logger.info("creating test user: " + loginName)

    def getRegistrationSecret(): String = {
      val path = basePath + "/account/check"
      val json = Json.obj("loginName" -> loginName)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get
      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "reservationSecret").as[String]
    }

    def register(regSec: String): String = {
      val path = basePath + "/account"
      val json = createUser(loginName, password, tel, email)  ++ Json.obj("reservationSecret" -> regSec)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get
      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }

      val data = (contentAsJson(res) \ "data").as[JsObject]
      val identity = (data \ "identities")(0).as[JsObject]
      (identity \ "id").as[String]
    }

    def getToken(): String = {
      val path = basePath + "/token"
      val auth = "Basic " + new sun.misc.BASE64Encoder().encode((loginName + ":" + password).getBytes)
      val req = FakeRequest(GET, path).withHeaders(("Authorization", auth))
      val res = route(req).get
      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "token").as[String]
    }

    val regSec = getRegistrationSecret()
    val identityId = register(regSec)
    val token = getToken()

    new TestUser(loginName, identityId, token)
  }

  def addExternalContact(token: String, tel: Option[String] = None, email: Option[String] = None): String = {

    Logger.info("adding external user")
    val path = basePath + "/contact"
    val identity =
      Json.obj("displayName" -> "moep") ++
        toJsonOrEmpty("phoneNumber", tel) ++
        toJsonOrEmpty("email", email)
    val json = Json.obj("identity" -> identity)

    val req = FakeRequest(POST, path).withHeaders(tokenHeader(token)).withJsonBody(json)
    val res = route(req).get
    if (status(res) != OK) {
      Logger.error("Response: " + contentAsString(res))
    }

    val data = (contentAsJson(res) \ "data").as[JsObject]
    (data \ "identityId").as[String]
  }

  def makeFriends(user1: TestUser, user2: TestUser): String = {

    Logger.info("making friends: " + user1.login + " and " + user2.login)

    def sendFriendRequest(): String = {
      val path = basePath + "/friendRequest"

      val json = Json.obj("identityId" -> user1.identityId, "message" -> "moep")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(user2.token)).withJsonBody(json)
      val res = route(req).get
      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }

      contentAsString(res)
    }

    def acceptFriendRequest(): String = {
      val path = basePath + "/friendRequest/answer"

      val json = Json.obj("answerType" -> "accept", "identityId" -> user2.identityId)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(user1.token)).withJsonBody(json)
      val res = route(req).get
      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }

      contentAsString(res)
    }

    sendFriendRequest()
    acceptFriendRequest()
  }
}
