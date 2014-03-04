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
class PurlController extends Specification {

  sequential

  val purlExtern = "MSaKlj4hJP"
  val purlExtern2 = "PkFWPuCiBB"
  val purlExtern3 = "agirsrEN3j"
  val purlExternIdentitityId = "GhEWGfy3Jqx8BRP1pITO"
  val purlIntern = "V3Ml6hzqX8"
  val purlIntern2 = "u02iLiIeQu"
  val purlConversationId = "OM9QeJ4RfJcdscyo52g4"
  var purlExternToken = ""

  "PurlController" should {

    step(play.api.Play.start(app))

    "get purl object for external user" in {

      val path = basePath + "/purl/" + purlExtern

      val req = FakeRequest(GET, path)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "conversation").asOpt[JsObject] must beSome
      val conversation = (data \ "conversation").as[JsObject]
      (data \ "identity").asOpt[JsObject] must beSome
      val identity = (data \ "identity").as[JsObject]
      (data \ "token").asOpt[String] must beSome
      purlExternToken = (data \ "token").as[String]

      (conversation \ "id").asOpt[String] must beSome(purlConversationId)
      (identity \ "id").asOpt[String] must beSome(purlExternIdentitityId)
    }

    "get purl object for external user with token" in {

      val path = basePath + "/purl/" + purlExtern

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(purlExternToken))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "conversation").asOpt[JsObject] must beSome
      val conversation = (data \ "conversation").as[JsObject]
      (conversation \ "id").asOpt[String] must beSome(purlConversationId)
      (data \ "identity").asOpt[JsObject] must beSome
      val identity = (data \ "identity").as[JsObject]
      (identity \ "id").asOpt[String] must beSome(purlExternIdentitityId)
      (data \ "token").asOpt[String] must beNone
    }

    "refuse to return purl object for external user with wrong token" in {
      val path = basePath + "/purl/" + purlExtern

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "get purl object of internal user with token" in {

      val path = basePath + "/purl/" + purlIntern

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "conversation").asOpt[JsObject] must beSome
      val conversation = (data \ "conversation").as[JsObject]
      (conversation \ "id").asOpt[String] must beSome(purlConversationId)

      (data \ "identity").asOpt[JsObject] must beSome
      val identity = (data \ "identity").as[JsObject]
      (identity \ "id").asOpt[String] must beSome(identityExisting2)

      (data \ "token").asOpt[String] must beNone
    }

    "refuse to get purl object of internal user with wrong token" in {

      val path = basePath + "/purl/" + purlIntern

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)

    }

    "refuse to get purl object of internal user with token of other member of the conversation" in {

      val path = basePath + "/purl/" + purlIntern

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(purlExternToken))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "refuse to get purl object of internal user without token" in {

      val path = basePath + "/purl/" + purlIntern

      val req = FakeRequest(GET, path)
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    step(play.api.Play.stop())
  }
}