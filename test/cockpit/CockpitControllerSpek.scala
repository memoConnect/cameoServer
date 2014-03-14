package cockpit

import play.api.libs.json.JsObject
import play.api.test.FakeRequest
import play.api.test.Helpers._
import testHelper.TestConfig._
import testHelper.MockupFactory._
import testHelper.StartedApp

/**
 * Created by dermicha on 12.03.14.
 */
class CockpitControllerSpec extends StartedApp {

  "CockpitEditController" should {

    "Get the identity behind a id" in {
      val id = "GhEWGfy3Jqx8BRP1pITO";
      val path = baseCockpitPath + "/identity/" + id

      val req = FakeRequest(GET, path)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").as[String] equals (id)
    }
  }
}

