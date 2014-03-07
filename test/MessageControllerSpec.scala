import java.text.SimpleDateFormat
import java.util.{ Date, Calendar }
import org.joda.time.DateTime
import play.api.libs.json.{ JsArray, Json, JsObject }
import play.api.test.{ FakeRequest, FakeApplication }
import play.api.test.Helpers._
import scala.Some
import testHelper.MockupFactory._
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import play.api.Logger
import testHelper.{ StartedApp, MockupFactory }
import org.specs2.mutable._
import testHelper.Config._

/**
 * User: Björn Reimer
 * Date: 3/3/14
 * Time: 4:48 PM
 */
class MessageControllerSpec extends StartedApp {

  sequential

  var messageId = ""
  var messageBody = "wir rocken"

  "MessageController" should {

    "add message to conversation" in {
      val path = basePath + "/conversation/" + cidExisting + "/message"

      val json = Json.obj("messageBody" -> messageBody)

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
      (data \ "messageBody").asOpt[String] must beSome(messageBody)
      (data \ "messageStatus").asOpt[Seq[JsObject]] must beSome
      (data \ "fromIdentity").asOpt[String] must beSome
      (data \ "created").asOpt[String] must beSome
    }

    "check if conversation summary is updated" in {
      val path = basePath + "/conversation/" + cidExisting + "/summary"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "messages")(0).asOpt[JsObject] must beSome

      val message = (data \ "messages")(0).as[JsObject]
      (message \ "id").asOpt[String] must beSome(messageId)
      (message \ "messageBody").asOpt[String] must beSome(messageBody)
      (message \ "messageStatus").asOpt[Seq[JsObject]] must beSome
      (message \ "fromIdentity").asOpt[String] must beSome(identityExisting)
      (message \ "created").asOpt[String] must beSome

      // check that lastUpdated contains todays date
      (data \ "lastUpdated").asOpt[String] must beSome(contain(new SimpleDateFormat("dd.MM.yyyy").format(new Date)))
    }

    "check if this conversation is returned first in the conversations call" in {
      val path = basePath + "/conversations"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "conversations")(0).asOpt[JsObject] must beSome
      val conversation = (data \ "conversations")(0).as[JsObject]

      (conversation \ "id").asOpt[String] must beSome(cidExisting)
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
  }
}