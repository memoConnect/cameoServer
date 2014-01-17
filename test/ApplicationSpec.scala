package test


import play.api.test._
import play.api.test.Helpers._

import org.specs2.mutable._
import play.api.libs.json.{Json, JsValue}
import play.api.libs.concurrent.Akka
import play.api.Logger
import play.api.mvc.AsyncResult


/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */

class ApplicationSpec extends Specification {

  "Account Controller" should {

    val account = {
      Json.obj("loginName" -> "testUser") ++
        Json.obj("password" -> "abcdefgh") ++
        Json.obj("email" -> "e@mail.de") ++
        Json.obj("phoneNumber" -> "01234567890")
    }

    "create Account" in new WithApplication {

      val req = FakeRequest(POST, "/api/v1/account").withJsonBody(account)
      val res = route(req).get

      status(res) must equalTo(OK)

      val json = (contentAsJson(res) \ "data").as[JsValue]

      (json \ "loginName").asOpt[String] must equalTo((account \ "loginName").asOpt[String])
      (json \ "email").asOpt[String] must equalTo((account \ "email").asOpt[String])
      (json \ "phoneNumber").asOpt[String] must equalTo((account \ "phoneNumber").asOpt[String])
      (json \ "identities").asOpt[JsValue] must not beEmpty

      Logger.info("######################" + Akka.system.isTerminated.toString)


    }


    "create Account with no email and phoneNumber" in new WithApplication {

      Logger.info("-------------------------------" + Akka.system.isTerminated.toString)


      val account2 = {
        Json.obj("loginName" -> "testUser2") ++
          Json.obj("password" -> "abcdefgh")
      }

      val req = FakeRequest(POST, "/api/v1/account").withJsonBody(account2)
      val res = route(req).get


      Logger.debug(res.toString)

      status(res) must equalTo(OK)

      val json = (contentAsJson(res) \ "data").as[JsValue]

      (json \ "loginName").asOpt[String] must equalTo((account2 \ "loginName").asOpt[String])
      (json \ "identities").asOpt[JsValue] must not beEmpty
    }
  }
}