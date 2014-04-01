import java.text.SimpleDateFormat
import java.util.{ Date, Calendar }
import org.joda.time.DateTime
import play.api.libs.json.{ JsArray, Json, JsObject }
import play.api.test.{ FakeRequest, FakeApplication }
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
class MessageControllerSpec extends StartedApp {

  sequential

  var messageId = ""
  var body = "wir rocken"

  "MessageController" should {

    "add message to conversation" in {
      val path = basePath + "/conversation/" + cidExisting2 + "/message"

      val json = Json.obj("body" -> body)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      messageId = (data \ "id").as[String]
      (data \ "body").asOpt[String] must beSome(body)
      (data \ "messageStatus").asOpt[Seq[JsObject]] must beSome
      (data \ "fromIdentity").asOpt[String] must beSome
      (data \ "created").asOpt[Long] must beSome
    }

    "get single message" in {
      val path = basePath + "/message/" + messageId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "body").asOpt[String] must beSome(body)
      (data \ "messageStatus").asOpt[Seq[JsObject]] must beSome
      (data \ "fromIdentity").asOpt[String] must beSome
      (data \ "created").asOpt[Long] must beSome
    }

    "check if conversation summary is updated" in {
      val path = basePath + "/conversation/" + cidExisting2 + "/summary"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "messages")(0).asOpt[JsObject] must beSome

      val message = (data \ "messages")(0).as[JsObject]
      (message \ "id").asOpt[String] must beSome(messageId)
      (message \ "body").asOpt[String] must beSome(body)
      (message \ "messageStatus").asOpt[Seq[JsObject]] must beSome
      (message \ "fromIdentity").asOpt[String] must beSome(identityExisting)
      (message \ "created").asOpt[Long] must beSome

      // check that lastUpdated contains todays date
      (data \ "lastUpdated").asOpt[Long] must beSome
    }

    "check if this conversation is returned first in the conversations call" in {
      val path = basePath + "/conversations"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "conversations")(0).asOpt[JsObject] must beSome
      val conversation = (data \ "conversations")(0).as[JsObject]

      (conversation \ "id").asOpt[String] must beSome(cidExisting2)
    }

    "refuse non-members to send message to conversation" in {

      val path = basePath + "/conversation/" + cidExisting2 + "/message"

      val json = Json.obj("body" -> "wir rocken")

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

    val fileIds = Seq("asdf", "aassddff")
    var messageId2 = ""

    "send message with fileIds" in {
      val path = basePath + "/conversation/" + cidExisting2 + "/message"

      val json = Json.obj("body" -> body , "fileIds" -> fileIds)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      Logger.debug("foo" + data)

      (data \ "id").asOpt[String] must beSome
      messageId2 = (data \ "id").as[String]
      (data \ "body").asOpt[String] must beSome(body)
      (data \ "messageStatus").asOpt[Seq[JsObject]] must beSome
      (data \ "fromIdentity").asOpt[String] must beSome
      (data \ "fileIds")(0).asOpt[String] must beSome(fileIds(0))
      (data \ "fileIds")(1).asOpt[String] must beSome(fileIds(1))
      (data \ "created").asOpt[Long] must beSome
    }

    "return fileIds with message" in {
      val path = basePath + "/message/" + messageId2

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "body").asOpt[String] must beSome(body)
      (data \ "messageStatus").asOpt[Seq[JsObject]] must beSome
      (data \ "fromIdentity").asOpt[String] must beSome
      (data \ "fileIds")(0).asOpt[String] must beSome(fileIds(0))
      (data \ "fileIds")(1).asOpt[String] must beSome(fileIds(1))
      (data \ "created").asOpt[Long] must beSome
    }
  }
}