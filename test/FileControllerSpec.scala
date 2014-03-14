import play.api.test._
import play.api.libs.json.{ Json, JsObject }
import play.api.test.Helpers._
import play.api.test.FakeApplication
import testHelper.MockupFactory._
import scala.concurrent.ExecutionContext
import play.api.GlobalSettings
import testHelper.{ StartedApp, MockupFactory }
import org.specs2.mutable._
import testHelper.TestConfig._

class FileControllerSpec extends StartedApp {

  sequential

  "FileController" should {

    val fileName = "some_name.pdf"
    val fileType = "some_type"
    val fileSize = 1234567
    var fileId = ""

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
          ("X-File-Type", fileType)) :+
          tokenHeader(tokenExisting2)

        val req = FakeRequest(POST, path).withHeaders(header: _*).withJsonBody(json)
        val res = route(req).get

        status(res) must equalTo(OK)

        val data = (contentAsJson(res) \ "data").as[JsObject]

        (data \ "id").asOpt[String] must beSome
        fileId = (data \ "id").as[String]
        (data \ "chunks")(0).asOpt[Int] must beNone
        (data \ "fileName").asOpt[String] must beSome(fileName)
        (data \ "maxChunks").asOpt[Int] must beSome(chunks.size)
        (data \ "fileSize").asOpt[Int] must beSome(fileSize)
        (data \ "fileType").asOpt[String] must beSome(fileType)
      }

      "upload the other chunks" in {
        val path = basePath + "/file/" + fileId

        chunks.zipWithIndex.map {
          case (chunk, i) =>

            val json = Json.obj("chunk" -> chunk)

            val header: Seq[(String, String)] = Seq(
              ("X-Index", i.toString)) :+
              tokenHeader(tokenExisting2)

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
          tokenHeader(tokenExisting2)

        val req = FakeRequest(POST, path).withHeaders(header: _*).withJsonBody(json)
        val res = route(req).get

        status(res) must equalTo(NOT_FOUND)
      }

      "get file meta information" in {

        val path = basePath + "/file/" + fileId

        val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
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

            val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
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

            val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
            val res = route(req).get

            status(res) must equalTo(NOT_FOUND)
          }
        }
      }

      "detect non-numerical chunk index" in {
        chunks.zipWithIndex.map {
          case (chunk, i) => {
            val path = basePath + "/file/" + fileId + "/d"

            val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
            val res = route(req).get

            status(res) must equalTo(BAD_REQUEST)
          }
        }
      }

      "refuse to return non existing FileMeta" in {
        val path = basePath + "/file/0"

        val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
        val res = route(req).get

        status(res) must equalTo(NOT_FOUND)
      }
    }
  }
}
