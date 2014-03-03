import play.api.libs.json.{JsArray, Json, JsObject}
import play.api.test.{FakeRequest, FakeApplication}
import play.api.test.Helpers._
import scala.Some
import testHelper.MockupFactory._
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import play.api.Logger
import testHelper.MockupFactory
import org.specs2.mutable._
import testHelper.Config._

/**
 * User: Björn Reimer
 * Date: 3/3/14
 * Time: 4:48 PM
 */
class MessageControllerSpec extends Specification {

  sequential

  var messageId = ""
  var messageBody = "wir rocken"

  "MessageController" should {

    step(play.api.Play.start(app))

    "add message to conversation" in {
      val path = basePath + "/conversation/" + cidExisting + "/message"

      val json = Json.obj("messageBody" -> "wir rocken")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      messageId = (data \ "id").as[String]
      (data \ "messageBody").asOpt[String] must beSome(messageBody)
      (data \ "messageStatus").asOpt[Seq[JsObject]] must beSome
      (data \ "fromIdentity").asOpt[String] must beSome
      (data \ "created").asOpt[String] must beSome
    }

    "get single message" in {
      val path = basePath + "/message/" + messageId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      messageId = (data \ "id").as[String]
      (data \ "messageBody").asOpt[String] must beSome("wir rocken")
      (data \ "messageStatus").asOpt[Seq[JsObject]] must beSome
      (data \ "fromIdentity").asOpt[String] must beSome
      (data \ "created").asOpt[String] must beSome
    }

    "refuse non-members to send message to conversation" in {

      val path = basePath + "/conversation/" + cidExisting + "/message"

      val json = Json.obj("messageBody" -> "wir rocken")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)

    }

    "refuse non-members to get single message" in {
      val path = basePath + "/message/" + messageId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }
    step(play.api.Play.stop())
  }
}