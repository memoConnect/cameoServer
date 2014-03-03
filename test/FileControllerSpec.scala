import play.api.libs.json.JsArray
import play.api.libs.json.JsObject
import play.api.test._
import play.api.libs.json.{ JsArray, Json, JsObject }
import play.api.test.FakeApplication
import play.api.test.Helpers._
import play.api.test.FakeApplication
import scala.Some
import testHelper.MockupFactory._
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import scala.concurrent.ExecutionContext
import play.api.{ GlobalSettings, Logger }
import helper.DbAdminUtilities
import testHelper.MockupFactory
import org.specs2.mutable._

class ControllerSpec extends Specification {

  val basePath = "/api/v1"
  val dbName = "cameo_test"

  sequential

  "Controller" should {

    val additionalConfig = Map("mongodb.db" -> dbName)

    // valid users in the inital Data: login;password;identityId;token
    //    BMeSfHXQ;password;N2HKgBdxxnWBGxlYY7Dn;viRlhZZ1VDAhqcgrljvfzEXCwKj0B2dyAKw5suFZ
    //    2VqTftqh;password;g9PWZY7xKNbeCO6LPNnx;hUbODA2qkVo2JF7YdEYVXe4NaHd82x6rvxxBxXbo

    // test user on dev.cameo.io
    // r1Zhpq8e;password;NyGAvBnLeR3mLEYdofgf;lFFkssj7gE4uTGSZlPlolp82Ozp3fWnOkQEFYO6k


    // Use the same FakeApplication for all tests, so the mongoConnection does not break
    lazy val app = FakeApplication(additionalConfiguration = additionalConfig)
    step(play.api.Play.start(app))

    val login = randomString(8)
    val login2 = randomString(8)
    val pass = randomString(8)
    val mail = "e@mail.de"
    val tel = "+491234567890"
    var identityId = ""
    var token = ""
    var regSec = ""
    var regSec2 = ""
    var cidNew = ""
    var identityOf10thContact = ""
    var idOf10thContact = ""
    var contactId = ""
    val newContactMail = "test@bjrm.de"
    val newContactTel = "+4561233"
    val newContactName = "foobar"
    val cidExisting = "rQHQZHv4ARDXRmnEzJ92"
    val cidOther = "2GOdNSfdPMavyl95KUah"
    val identityExisting = "g9PWZY7xKNbeCO6LPNnx"
    val identityExisting2 = "N2HKgBdxxnWBGxlYY7Dn"
    val validRecipients = Seq("6iOuCefN12ma0wF7QxR5", "dKeg67XtSNBCFMq8WQor")
    val recipientMemberOfConversation = "Tya0cZiaYFhFOBS2RNP1"
    val purlExtern = "MSaKlj4hJP"
    val purlExtern2 = "PkFWPuCiBB"
    val purlExtern3 = "agirsrEN3j"
    val purlExternIdentitityId = "GhEWGfy3Jqx8BRP1pITO"
    val purlIntern = "V3Ml6hzqX8"
    val purlIntern2 = "u02iLiIeQu"
    val purlConversationId = "OM9QeJ4RfJcdscyo52g4"

    val token2 = "hUbODA2qkVo2JF7YdEYVXe4NaHd82x6rvxxBxXbo"
    val token3 = "viRlhZZ1VDAhqcgrljvfzEXCwKj0B2dyAKw5suFZ"

    val chunks: Seq[String] = {
      Seq.fill(10)(MockupFactory.randomString(256))
    }

  "FileController" should {
    "upload first chunk of file" in {
      val path = basePath + "/file"

      val json = Json.obj("chunk" -> chunks.head)

      val header: Seq[(String, String)] = Seq(
        ("X-File-Name", fileName),
        ("X-Max-Chunks", chunks.size.toString),
        ("X-File-Size", fileSize.toString),
        ("X-File-Type", fileType),
        ("X-Index", "0")) :+
        tokenHeader(token2)

      val req = FakeRequest(POST, path).withHeaders(header: _*).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      fileId = (data \ "id").as[String]
      (data \ "chunks")(0).asOpt[Int] must beSome
      (data \ "chunks")(1).asOpt[Int] must beNone
      (data \ "fileName").asOpt[String] must beSome(fileName)
      (data \ "maxChunks").asOpt[Int] must beSome(chunks.size)
      (data \ "fileSize").asOpt[Int] must beSome(fileSize)
      (data \ "fileType").asOpt[String] must beSome(fileType)
    }

    "upload the other chunks" in {
      val path = basePath + "/file/" + fileId

      chunks.tail.zipWithIndex.map {
        case (chunk, i) =>

          val json = Json.obj("chunk" -> chunk)

          val header: Seq[(String, String)] = Seq(
            ("X-Index", (i + 1).toString)) :+
            tokenHeader(token2)

          val req = FakeRequest(POST, path).withHeaders(header: _*).withJsonBody(json)
          val res = route(req).get

          status(res) must equalTo(OK)
      }
    }

    "refuse upload to chunks to invalid fileId" in {
      val path = basePath + "/file/" + "asdfasdf"

      val json = Json.obj("chunk" -> chunks(1))

      val header: Seq[(String, String)] = Seq(
        ("X-Index", "2")) :+
        tokenHeader(token)

      val req = FakeRequest(POST, path).withHeaders(header: _*).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(NOT_FOUND)
    }

    "get file meta information" in  {

      val path = basePath + "/file/" + fileId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      fileId = (data \ "id").as[String]
      (data \ "chunks").asOpt[Seq[Int]] must beSome
      val returnedChunks: Seq[Int] = (data \ "chunks").as[Seq[Int]].sorted
      returnedChunks.size must beEqualTo(10)
      returnedChunks.min must beEqualTo(0)
      returnedChunks.max must beEqualTo(9)
      (data \ "fileName").asOpt[String] must beSome(fileName)
      (data \ "maxChunks").asOpt[Int] must beSome(chunks.size)
      (data \ "fileSize").asOpt[Int] must beSome(fileSize)
      (data \ "fileType").asOpt[String] must beSome(fileType)
    }

    "get all chunks of a file" in {
      chunks.zipWithIndex.map {
        case (chunk, i) => {
          val path = basePath + "/file/" + fileId + "/" + i

          val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
          val res = route(req).get

          status(res) must equalTo(OK)

          val data = (contentAsJson(res) \ "data").as[JsObject]

          (data \ "chunk").asOpt[String] must beSome(chunks(i))
        }
      }
    }

    "refuse to return non existing chunk" in {
      chunks.zipWithIndex.map {
        case (chunk, i) => {
          val path = basePath + "/file/" + fileId + "/15"

          val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
          val res = route(req).get

          status(res) must equalTo(NOT_FOUND)
        }
      }
    }

    "detect non-numerical chunk index" in {
      chunks.zipWithIndex.map {
        case (chunk, i) => {
          val path = basePath + "/file/" + fileId + "/d"

          val req = FakeRequest(GET, path).withHeaders(tokenHeader(token2))
          val res = route(req).get

          status(res) must equalTo(BAD_REQUEST)
        }
      }
    }

    "refuse to return non existing FileMeta" in {
      val path = basePath + "/file/0"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(token3))
      val res = route(req).get

      status(res) must equalTo(NOT_FOUND)
    }
  }

}
