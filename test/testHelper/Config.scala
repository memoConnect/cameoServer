package testHelper

import play.api.test.FakeApplication
import testHelper.MockupFactory._
import play.api.test.FakeApplication

/**
 * User: Björn Reimer
 * Date: 3/3/14
 * Time: 4:41 PM
 */


object Config {

  val basePath = "/api/v1"
  val dbName = "cameo_test"

  // valid users in the inital Data: login;password;identityId;token
  // 2VqTftqh;password;g9PWZY7xKNbeCO6LPNnx;hUbODA2qkVo2JF7YdEYVXe4NaHd82x6rvxxBxXbo
  // BMeSfHXQ;password;N2HKgBdxxnWBGxlYY7Dn;viRlhZZ1VDAhqcgrljvfzEXCwKj0B2dyAKw5suFZ

  // test user on dev.cameo.io
  // r1Zhpq8e;password;NyGAvBnLeR3mLEYdofgf;lFFkssj7gE4uTGSZlPlolp82Ozp3fWnOkQEFYO6k

  // Use the same FakeApplication for all tests, so the mongoConnection does not break
  val additionalConfig = Map("mongodb.db" -> dbName, "logger.application" -> "ERROR", "logger.play" -> "ERROR")
  lazy val app = FakeApplication(additionalConfiguration = additionalConfig)

  val cidExisting = "rQHQZHv4ARDXRmnEzJ92"
  val cidOther = "2GOdNSfdPMavyl95KUah"

  val identityExisting = "g9PWZY7xKNbeCO6LPNnx"
  val identityExisting2 = "N2HKgBdxxnWBGxlYY7Dn"


  val tokenExisting = "hUbODA2qkVo2JF7YdEYVXe4NaHd82x6rvxxBxXbo"
  val tokenExisting2 = "viRlhZZ1VDAhqcgrljvfzEXCwKj0B2dyAKw5suFZ"

  val telExisting= "+49123456789"
  val emailExisting= "test@cameo.io"

}
