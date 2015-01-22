package testHelper

import org.specs2.matcher.MatchResult
import play.api.Logger
import play.api.libs.json.{ Json, JsObject }
import helper.TestValueStore
import play.api.test.FakeRequest
import play.api.test.Helpers._
import testHelper.TestConfig._
import play.api.test.Helpers._
import org.specs2.matcher.{ MatchResult, Matcher, SomeMatcher }


/**
 * User: Björn Reimer
 * Date: 1/30/14
 * Time: 2:19 PM
 */
object Helper {

  def toJsonOrEmpty(key: String, value: Option[String]): JsObject = {
    value.fold(Json.obj())(v => Json.obj(key -> v))
  }

  def createUser(login: String, password: String, tel: Option[String] = None, email: Option[String] = None): JsObject = {
    Json.obj(
      "loginName" -> login,
      "password" -> password) ++
      toJsonOrEmpty("phoneNumber", tel) ++
      toJsonOrEmpty("email", email)
  }

  val random = new scala.util.Random

  def randomString(n: Int): String = {
    def alphabet: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    //TODO check whether random.setSeed is needed
    Stream.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString
  }

  def randomLengthString(max: Int) = {
    val n = random.nextInt(max)
    val nonZero = if (n == 0) 1 else n
    randomString(nonZero)
  }

  def waitFor(test: => Boolean) {
    var maxRepetitions = 100
    while (!test && maxRepetitions > 0) {
      maxRepetitions -= 1
      Thread.sleep(50)
      if (maxRepetitions % 20 == 0) {
        Logger.debug("waiting: " + test)
      }
    }
  }



}
