
import play.api.Logger
import play.api.test._
import play.api.libs.json.{ Json, JsObject }
import play.api.test.Helpers._
import testHelper.Stuff._
import org.specs2.mutable._
import testHelper.TestConfig._
import testHelper.StartedApp

/**
 * User: Björn Reimer
 * Date: 3/3/14
 * Time: 4:48 PM
 */
class IdentityControllerSpec extends StartedApp {

  sequential

  val newTel = validPhoneNumbers(1)._1
  val newTelCleaned = validPhoneNumbers(1)._2
  val newMail = validEmails(1)
  val newName = "newNameasdfasdf"

  var cameoId = ""

  val pubKey = "asdfasdfasdf"
  val pubKeyName = "moep"
  var pubKeyId = ""
  val pubKeySize = 15
  val newPubKey = "woops"
  val newPubKeyName = "poem"
  val newPubKeySize = 454

  val pubKey2 = "asdfasdfasdf2"
  val pubKeyName2 = "moep2"
  var pubKeyId2 = ""
  val pubKeySize2 = 2048


  "IdentityController" should {

    "Get the identity behind a token" in {
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "userKey").asOpt[String] must beSome
      (data \ "cameoId").asOpt[String] must beSome
      cameoId = (data \ "cameoId").as[String]
      (data \ "email" \ "value").asOpt[String] must beSome(emailExisting)
      (data \ "phoneNumber" \ "value").asOpt[String] must beSome(telExisting)
    }

    "Edit an identity" in {

      val path = basePath + "/identity"

      val json = Json.obj("phoneNumber" -> newTel, "email" -> newMail, "displayName" -> newName)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "check if identity was edited" in {

      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "phoneNumber" \ "value").asOpt[String] must beSome(newTelCleaned)
      (data \ "phoneNumber" \ "isVerified").asOpt[Boolean] must beSome(false)
      (data \ "email" \ "value").asOpt[String] must beSome(newMail)
      (data \ "email" \ "isVerified").asOpt[Boolean] must beSome(false)
      (data \ "displayName").asOpt[String] must beSome(newName)

    }

    "refuse to add invalid email" in {
      invalidEmails.map { invalid =>

        val path = basePath + "/identity"

        val json = Json.obj("email" -> invalid)

        val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
        val res = route(req).get

        status(res) must equalTo(BAD_REQUEST)
      }
    }

    "refuse to add invalid phoneNumber" in {
      invalidPhoneNumbers.map { invalid =>

        val path = basePath + "/identity"

        val json = Json.obj("phoneNumber" -> invalid)

        val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
        val res = route(req).get

        status(res) must equalTo(BAD_REQUEST)
      }
    }

    "Search for an CameoId" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> cameoId, "fields" -> Seq("cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "cameoId").asOpt[String] must beSome(cameoId)
      (data(0) \ "id").asOpt[String] must beSome(identityExisting)

      data.length must beEqualTo(1)
    }

    "Search for an CameoId with substring" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> cameoId.substring(0, 4), "fields" -> Seq("cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "cameoId").asOpt[String] must beSome(cameoId)
      (data(0) \ "id").asOpt[String] must beSome(identityExisting)

      data.length must beEqualTo(1)
    }

    "Refuse to Search for an CameoId if the search term is too short" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> "abc", "fields" -> Seq("cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
    }

    "Find nothing for non-existing CameoIds" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> "moep", "fields" -> Seq("cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[String] = (contentAsJson(res) \ "data").as[Seq[String]]

      data.length must beEqualTo(0)
    }

    "Search for a DisplayName" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> newName, "fields" -> Seq("displayName"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "id").asOpt[String] must beSome(identityExisting)

      data.length must beEqualTo(1)
    }

    "Search for DisplayName with substring" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> newName.substring(2, 6), "fields" -> Seq("displayName"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "id").asOpt[String] must beSome(identityExisting)

      data.length must beEqualTo(1)
    }

    "Find nothing for non-existing DisplayNames" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> "moep", "fields" -> Seq("displayName"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[String] = (contentAsJson(res) \ "data").as[Seq[String]]

      data.length must beEqualTo(0)
    }

    "Search for DisplayName or CameoId 1 " in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> newName.substring(2, 6), "fields" -> Seq("displayName", "cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "id").asOpt[String] must beSome(identityExisting)

      data.length must beEqualTo(1)
    }

    "Search for DisplayName or CameoId 2 " in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> cameoId.substring(0, 4), "fields" -> Seq("cameoId", "displayName"))

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "cameoId").asOpt[String] must beSome(cameoId)
      (data(0) \ "id").asOpt[String] must beSome(identityExisting)

      data.length must beEqualTo(1)
    }

    "add public key to identity" in {
      val path = basePath + "/identity/publicKey"

      val json = Json.obj("name" -> pubKeyName, "key" -> pubKey, "keySize" -> pubKeySize)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      pubKeyId = (data \ "id").as[String]
      (data \ "name").asOpt[String] must beSome(pubKeyName)
      (data \ "key").asOpt[String] must beSome(pubKey)
      (data \ "keySize").asOpt[Int] must beSome(pubKeySize)
    }

    "add another public key to identity" in {
      val path = basePath + "/identity/publicKey"

      val json = Json.obj("name" -> pubKeyName2, "key" -> pubKey2, "keySize" -> pubKeySize2)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      pubKeyId2 = (data \ "id").as[String]
      (data \ "name").asOpt[String] must beSome(pubKeyName2)
      (data \ "key").asOpt[String] must beSome(pubKey2)
      (data \ "keySize").asOpt[Int] must beSome(pubKeySize2)

    }

    "check if identity contains both added public keys" in {

      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "publicKeys").asOpt[Seq[JsObject]] must beSome
      val pubKeys = (data \ "publicKeys").as[Seq[JsObject]]

      pubKeys.length must beEqualTo(2)

      val key1: JsObject = pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId)).get
      (key1 \ "id").asOpt[String] must beSome(pubKeyId)
      (key1 \ "name").asOpt[String] must beSome(pubKeyName)
      (key1 \ "key").asOpt[String] must beSome(pubKey)
      (key1 \ "keySize").asOpt[Int] must beSome(pubKeySize)

      val key2: JsObject = pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId2)).get
      (key2 \ "id").asOpt[String] must beSome(pubKeyId2)
      (key2 \ "name").asOpt[String] must beSome(pubKeyName2)
      (key2 \ "key").asOpt[String] must beSome(pubKey2)
      (key2 \ "keySize").asOpt[Int] must beSome(pubKeySize2)

    }

    "edit name of public key" in {

      val path = basePath + "/identity/publicKey/" + pubKeyId

      val json = Json.obj("name" -> newPubKeyName)

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "edit public key" in {

      val path = basePath + "/identity/publicKey/" + pubKeyId

      val json = Json.obj("key" -> newPubKey, "keySize" -> newPubKeySize)

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "check if public key was edited" in {
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "publicKeys").asOpt[Seq[JsObject]] must beSome
      val pubKeys = (data \ "publicKeys").as[Seq[JsObject]]

      pubKeys.length must beEqualTo(2)

      val key1: JsObject = pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId)).get
      (key1 \ "id").asOpt[String] must beSome(pubKeyId)
      (key1 \ "name").asOpt[String] must beSome(newPubKeyName)
      (key1 \ "key").asOpt[String] must beSome(newPubKey)
      (key1 \ "keySize").asOpt[Int] must beSome(newPubKeySize)

      val key2: JsObject = pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId2)).get
      (key2 \ "id").asOpt[String] must beSome(pubKeyId2)
      (key2 \ "name").asOpt[String] must beSome(pubKeyName2)
      (key2 \ "key").asOpt[String] must beSome(pubKey2)
      (key2 \ "keySize").asOpt[Int] must beSome(pubKeySize2)

    }

    "delete a public key" in {
      val path = basePath + "/identity/publicKey/" + pubKeyId

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "check if key was deleted" in {
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "publicKeys").asOpt[Seq[JsObject]] must beSome
      val pubKeys = (data \ "publicKeys").as[Seq[JsObject]]

      pubKeys.length must beEqualTo(1)

      pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId)) must beNone

      val key2: JsObject = pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId2)).get
      (key2 \ "id").asOpt[String] must beSome(pubKeyId2)
      (key2 \ "name").asOpt[String] must beSome(pubKeyName2)
      (key2 \ "key").asOpt[String] must beSome(pubKey2)
      (key2 \ "keySize").asOpt[Int] must beSome(pubKeySize2)

    }

  }
}
