import org.specs2.execute.{ AsResult, Result }
import org.specs2.specification.{ Example, Fragment }
import play.api.libs.json.JsObject
import play.api.libs.json.JsObject
import play.api.libs.json.JsObject
import play.api.libs.json.{ Json, JsObject }
import play.api.Logger
import play.api.mvc.SimpleResult
import play.api.test.FakeRequest
import play.api.test.Helpers._
import scala.concurrent.Future
import scala.Some
import scala.util.Random
import testHelper.StartedApp
import testHelper.Stuff._
import testHelper.TestConfig._
import helper.JsonHelper._

/**
 * User: Björn Reimer
 * Date: 30.04.14
 * Time: 11:47
 */
class CallStackControllerSpec extends StartedApp {

  sequential

  def testCallStack(calls: Seq[Call], token: String): Result = {

    val path = basePath + "/callStack"
    val body = Json.obj("requests" -> calls.map(_.toRequestJson))
    val req = FakeRequest(POST, path).withJsonBody(body).withHeaders(tokenHeader(token))
    val res = route(req).get
    status(res) must equalTo(OK)

//    Logger.debug("RES: " + contentAsJson(res))

    val responses = (contentAsJson(res) \ "data" \ "responses").as[Seq[JsObject]]

    responses.length must equalTo(calls.length)

    val results = responses.zip(calls).map {
      case (js, call) =>
        (js \ "status").asOpt[Int] must beSome(call.expectedStatus)
        (js \ "body").asOpt[JsObject] must beSome
        call.checkResult((js \ "body").as[JsObject])
    }
    results.head
  }

  case class Call(path: String, method: String, body: Option[JsObject], expectedStatus: Int, checkResult: JsObject => Result) {
    def toRequestJson: JsObject = Json.obj(
      "path" -> (basePath + path),
      "method" -> method) ++
      maybeEmptyJsValue("data", body)
  }

  val getIdentity = new Call("/identity", "get", None, OK, { js =>
    val data = (js \ "data").as[JsObject]
    (data \ "id").asOpt[String] must beSome
    (data \ "userKey").asOpt[String] must beSome
    (data \ "cameoId").asOpt[String] must beSome
    (data \ "email" \ "value").asOpt[String] must beSome(emailExisting)
    (data \ "phoneNumber" \ "value").asOpt[String] must beSome(telExisting)
  })

  val getPurl = new Call("/purl/" + purlExtern, "GET", None, OK, { js =>

    val data = (js \ "data").as[JsObject]

    (data \ "conversation").asOpt[JsObject] must beSome
    val conversation = (data \ "conversation").as[JsObject]
    (data \ "identity").asOpt[JsObject] must beSome
    val identity = (data \ "identity").as[JsObject]
    (data \ "token").asOpt[String] must beSome

    (conversation \ "id").asOpt[String] must beSome(purlConversationId)
    (conversation \ "recipients").asOpt[Seq[JsObject]] must beSome
    val r = (conversation \ "recipients")(0).as[JsObject]
    (r \ "identityId").asOpt[String] must beSome
    (r \ "identity").asOpt[JsObject] must beSome
    (identity \ "id").asOpt[String] must beSome(purlExternIdentitityId)
  })

  val getIdentityUnauthorized = new Call("/identity", "get", None, UNAUTHORIZED, js => 1 === 1)

  val invalidPath = new Call("/moep", "get", None, NOT_FOUND, js => 1 === 1)
  val invalidMethod = new Call("/identity", "foo", None, NOT_FOUND, js => 1 === 1)

  val body = "moepDieMoep!"
  val sendMessage = new Call("/conversation/" + cidExisting2 + "/message", "post", Some(Json.obj("plain" -> Json.obj("text" -> body))), OK, { js =>
    val data = (js \ "data").as[JsObject]
    (data \ "id").asOpt[String] must beSome
    (data \ "plain" \ "text").asOpt[String] must beSome(body)
    (data \ "encrypted").asOpt[String] must beNone
    (data \ "messageStatus").asOpt[Seq[JsObject]] must beNone
    (data \ "fromIdentity").asOpt[String] must beSome(identityExisting)
    (data \ "created").asOpt[Long] must beSome
  })

  val invalidPostBody = new Call("/identity/search", "pOst", Some(Json.obj("foo" -> "baa")), BAD_REQUEST, { js =>
    (js \ "error").asOpt[JsObject] must beSome
  }
  )

  val updateIdentity = new Call("/identity", "puT", Some(Json.obj("email" -> emailExisting)), OK, { js =>
    (js \ "data").asOpt[String] must beSome("updated")
  })

  val deleteContact = new Call("/contact/" + externalContact2, "DELETE", None, OK, js => 1 === 1)

  "CallStackController" should {

    "should return not Found on invalid path" in {
      val stack = Seq(invalidPath)
      testCallStack(stack, tokenExisting)
    }

    "should return error on invalid method" in {
      val stack = Seq(invalidMethod)
      testCallStack(stack, tokenExisting)
    }

    "should process single GET request" in {
      val stack = Seq(getIdentity)
      testCallStack(stack, tokenExisting)
    }

    "should return unauthorized with invalid token" in {
      val stack = Seq(getIdentityUnauthorized)
      testCallStack(stack, "moep")
    }

    "allow calls that do not require authorization" in {
      val stack = Seq(getPurl)
      testCallStack(stack, "moep")
    }

    "should process single POST request" in {
      val stack = Seq(sendMessage)
      testCallStack(stack, tokenExisting)
    }

    "should return error on invalid json body" in {
      val stack = Seq(invalidPostBody)
      testCallStack(stack, tokenExisting)
    }

    "should process single PUT request" in {
      val stack = Seq(updateIdentity)
      testCallStack(stack, tokenExisting)
    }

    "should process single DELETE request" in {
      val stack = Seq(deleteContact)
      testCallStack(stack, tokenExisting2)
    }

    val all = Seq(invalidPath,
      invalidMethod,
      getIdentity,
      getPurl,
      sendMessage,
      invalidPostBody,
      updateIdentity
    )

    val large = Seq.fill(7)("foo").flatMap(s => all)

    "should execute all in one call" in {
      testCallStack(all, tokenExisting)
    }

    "should execute all in random order" in {
      testCallStack(Random.shuffle(all), tokenExisting)
    }

    "should execute a large call stack" in {
      testCallStack(large, tokenExisting)
    }

    "should execute a large random call stack" in {
      testCallStack(Random.shuffle(large), tokenExisting)
    }

    "should refuse if call stack is too long" in {
      val stack = Seq.fill(51)(getIdentity)
      val path = basePath + "/callStack"
      val body = Json.obj("requests" -> stack.map(_.toRequestJson))
      val req = FakeRequest(POST, path).withJsonBody(body)
      val res = route(req).get
      status(res) must equalTo(BAD_REQUEST)

      (contentAsJson(res) \ "error" \ "maxLength").asOpt[Int] must beSome(50)
    }

  }
}
