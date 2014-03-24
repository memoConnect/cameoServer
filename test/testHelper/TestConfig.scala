package testHelper

import play.api.test.FakeApplication
import testHelper.Stuff._
import play.api.test.FakeApplication
import play.api.{Logger, Play, GlobalSettings}
import akka.actor.Props
import actors.SendSmsActor
import info.schleichardt.play.embed.mongo.DynamicEmbedMongoPort
import play.api.mvc.EssentialAction
import play.api.http.HeaderNames._
import play.api.test.FakeApplication
import scala.Some
import scala.concurrent.{Await, Future}
import helper.MongoCollections._
import play.api.test.FakeApplication
import scala.Some
import play.api.libs.json.{JsValue, Json}
import helper.DbAdminUtilities
import models.GlobalState
import scala.concurrent.duration._

/**
 * User: Björn Reimer
 * Date: 3/3/14
 * Time: 4:41 PM
 */

object TestConfig {

  val basePath = "/api/v1"
  val baseCockpitPath = "/api/cockpit/v1"
  val dbName = "cameo_test"

  // valid users in the inital Data: login;password;identityId;token
  // 2VqTftqh;password;g9PWZY7xKNbeCO6LPNnx;hUbODA2qkVo2JF7YdEYVXe4NaHd82x6rvxxBxXbo
  // BMeSfHXQ;password;N2HKgBdxxnWBGxlYY7Dn;viRlhZZ1VDAhqcgrljvfzEXCwKj0B2dyAKw5suFZ

  // test user on dev.cameo.io
  // r1Zhpq8e;password;NyGAvBnLeR3mLEYdofgf;lFFkssj7gE4uTGSZlPlolp82Ozp3fWnOkQEFYO6k

  // Use the same FakeApplication for all tests, so the mongoConnection does not break
  val additionalConfig = Map("mongodb.db" -> dbName)
  val additionalConfigWithLoggingDisabled = Map("mongodb.db" -> dbName, "logger.application" -> "ERROR", "logger.play" -> "ERROR")

  lazy val app = FakeApplication(additionalConfiguration = additionalConfig)

  val cidExisting = "rQHQZHv4ARDXRmnEzJ92"
  val cidOther = "2GOdNSfdPMavyl95KUah"

  val identityExisting = "g9PWZY7xKNbeCO6LPNnx"
  val identityExisting2 = "N2HKgBdxxnWBGxlYY7Dn"

  val cameoIdExisting = "KG5mSGTY8l3"
  val cameoIdExisting2 ="bwyVeVnCvuO"

  val tokenExisting = "hUbODA2qkVo2JF7YdEYVXe4NaHd82x6rvxxBxXbo"
  val tokenExisting2 = "viRlhZZ1VDAhqcgrljvfzEXCwKj0B2dyAKw5suFZ"

  val telExisting = "+49123456789"
  val emailExisting = "test@cameo.io"

  val invalidPhoneNumbers = Seq("abcd", "+4912345123451234512345", "", "+!\"§$%&/()=")
  val invalidEmails = Seq("a@a.d", "a@a", "a@a aa.de", "a.de", "123@345.43")

  val validPhoneNumbers: Seq[(String, String)] =
    Seq(
      (" 0173-12  34dd5678"     , "+4917312345678"),
      ("491234512345"           , "+491234512345"),
      ("(0049)1234512345"       , "+491234512345"),
      ("0123/4512345"           , "+491234512345"),
      ("0123-4512345"           , "+491234512345"),
      (" +17234512345         " , "+17234512345")
    )
  val validEmails: Seq[String] = Seq("a-b.c_d@a-b.c_d.co", "123@345.fo", "123@3-4-5.fo")
}
