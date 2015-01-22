package testHelper

import org.specs2.matcher.{ AlwaysMatcher, MatchResult }
import org.specs2.mutable.{ BeforeAfter, Before, Specification }
import play.api.mvc.AnyContent
import play.api.{ Logger, Play }
import play.api.libs.json.{Json, JsValue, JsObject}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import testHelper.TestConfig._

/**
 * User: Björn Reimer
 * Date: 3/5/14
 * Time: 11:13 PM
 */
trait StartedApp extends Specification with BeforeAfter {

  override def before = {
    // check if app is started. start it if not
    Play.maybeApplication match {
      case Some(a) =>
      case None =>
        Play.start(TestConfig.app)
    }
  }

  override def after = {}

  def tokenHeader(token: String): (String, String) = ("Authorization", token)

  def twoFactorTokenHeader(token: String): (String, String) = ("X-TwoFactorToken", token)

  def executeRequest(method: String,
                     path: String,
                     expectedResult: Int,
                     token: Option[String] = None,
                     body: Option[JsObject] = None): JsValue = {

    val basicRequest = FakeRequest(method, basePath + path)

    val result = (token, body) match {
      case (None, None)       => route(basicRequest).get
      case (Some(t), None)    => route(basicRequest.withHeaders(tokenHeader(t))).get
      case (None, Some(b))    => route(basicRequest.withJsonBody(b)).get
      case (Some(t), Some(b)) => route(basicRequest.withHeaders(tokenHeader(t)).withJsonBody(b)).get
    }

    if (status(result) != expectedResult) {
      Logger.error("Response: " + contentAsString(result))
    }
    status(result) must equalTo(expectedResult)

    contentAsJson(result)
  }

  def getData(json: JsValue): JsObject = {
    val maybeData = (json \ "data").asOpt[JsObject]
    maybeData must beSome
    maybeData.get
  }

  def getDataSeq(json: JsValue): Seq[JsObject] = {
    val maybeData = (json \ "data").asOpt[Seq[JsObject]]
    maybeData must beSome
    maybeData.get
  }

  def checkError(json: JsValue) = {
    (json \ "res").asOpt[String] must beSome("KO")
  }

  def checkOk(json: JsValue) = {
    (json \ "res").asOpt[String] must beSome("OK")
  }

  case class TestUser(login: String, identityId: String, token: String, cameoId: String, displayName: String) {

    def addExternalContact(tel: Option[String] = None, email: Option[String] = None): (String, String) = {

      Logger.info("adding external user")
      val path = basePath + "/contact"
      val identity =
        Json.obj("displayName" -> "moep") ++
          Helper.toJsonOrEmpty("phoneNumber", tel) ++
          Helper.toJsonOrEmpty("email", email)
      val json = Json.obj("identity" -> identity)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(this.token)).withJsonBody(json)
      val res = route(req).get
      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }

      val data = (contentAsJson(res) \ "data").as[JsObject]
      val identityId = (data \ "identityId").as[String]
      val contactId = (data \ "id").as[String]

      (contactId, identityId)
    }

    def makeFriends(user2: TestUser): Int = {

      def sendFriendRequest(): Int = {
        val path = basePath + "/friendRequest"

        val json = Json.obj("identityId" -> user2.identityId, "message" -> "moep")

        val req = FakeRequest(POST, path).withHeaders(tokenHeader(this.token)).withJsonBody(json)
        val res = route(req).get
        if (status(res) != OK) {
          Logger.error("Response: " + contentAsString(res))
        }

        status(res)
      }

      def acceptFriendRequest(): Int = {
        val path = basePath + "/friendRequest/answer"

        val json = Json.obj("answerType" -> "accept", "identityId" -> this.identityId)

        val req = FakeRequest(POST, path).withHeaders(tokenHeader(user2.token)).withJsonBody(json)
        val res = route(req).get
        if (status(res) != OK) {
          Logger.error("Response: " + contentAsString(res))
        }

        status(res)
      }

      sendFriendRequest()
      acceptFriendRequest()
    }
  }

  object TestUser {

    def create(tel: Option[String] = None, email: Option[String] = None): TestUser = {

      val loginName = testUserPrefix + "_" + Helper.randomString(6)
      val displayName = Helper.randomString(8)

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

      def register(regSec: String): (String, String) = {
        val path = basePath + "/account"
        val json = Helper.createUser(loginName, password, tel, email) ++ Json.obj("reservationSecret" -> regSec) ++ Json.obj()

        val req = FakeRequest(POST, path).withJsonBody(json)
        val res = route(req).get
        if (status(res) != OK) {
          Logger.error("Response: " + contentAsString(res))
        }

        val data = (contentAsJson(res) \ "data").as[JsObject]
        val identity = (data \ "identities")(0).as[JsObject]
        val id = (identity \ "id").as[String]
        val cameoId = (identity \ "cameoId").as[String]
        (id, cameoId)
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
      val (identityId, cameoId) = register(regSec)
      val token = getToken()

      TestUser(loginName, identityId, token, cameoId, displayName)
    }
  }
}
