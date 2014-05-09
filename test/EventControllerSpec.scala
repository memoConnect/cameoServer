import play.api.libs.json.JsObject
import play.api.{Logger, Play}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import testHelper.{TestConfig, StartedApp}
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

  "EventController" should {

    "Get a new subscription Id" in {
      val path = basePath + "/eventSubscription"

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting))
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
      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting2))

      (1 to max).seq.foreach{ i =>
        val res = route(req).get
        i match {
          case j if j >= max =>  status(res) must equalTo(BAD_REQUEST)
          case _ =>  status(res) must equalTo(OK)
        }
      }

      1 === 1
    }


  }



}
