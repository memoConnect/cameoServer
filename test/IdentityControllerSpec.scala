
import play.api.Logger
import play.api.test._
import play.api.libs.json.{ JsString, Json, JsObject }
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
  val newPubKeyName = "poem"

  val pubKey2 = "asdfasdfasdf2"
  val pubKeyName2 = "moep2"
  var pubKeyId2 = ""
  val pubKeySize2 = 2048

  val externalContactDisplayName = "generic Display Name 15y"

  "IdentityController" should {

    "Get the identity behind a token" in {
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
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

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "check if identity was edited" in {

      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
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

      val json = Json.obj("search" -> cameoIdExisting2, "fields" -> Seq("cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "cameoId").asOpt[String] must beSome(cameoIdExisting2)
      (data(0) \ "id").asOpt[String] must beSome(identityExisting2)

      data.length must beEqualTo(1)
    }

    "Search for an CameoId with substring" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> cameoIdExisting2.substring(0, 4), "fields" -> Seq("cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "cameoId").asOpt[String] must beSome(cameoIdExisting2)
      (data(0) \ "id").asOpt[String] must beSome(identityExisting2)

      data.length must beEqualTo(1)
    }

    "Search for an CameoId with different case" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> cameoIdExisting2.toLowerCase, "fields" -> Seq("cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "cameoId").asOpt[String] must beSome(cameoIdExisting2)
      (data(0) \ "id").asOpt[String] must beSome(identityExisting2)

      data.length must beEqualTo(1)
    }

    "Refuse to Search for an CameoId if the search term is too short" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> "abc", "fields" -> Seq("cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(BAD_REQUEST)
    }

    "Find nothing for non-existing CameoIds" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> "moeps", "fields" -> Seq("cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data: Seq[String] = (contentAsJson(res) \ "data").as[Seq[String]]

      data.length must beEqualTo(0)
    }

    "Search for a DisplayName" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> displayNameExisting2, "fields" -> Seq("displayName"))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "id").asOpt[String] must beSome(identityExisting2)

      data.length must beEqualTo(1)
    }

    "Search for DisplayName with substring" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> displayNameExisting2.substring(2, 6), "fields" -> Seq("displayName"))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "id").asOpt[String] must beSome(identityExisting2)

      data.length must beEqualTo(1)
    }

    "Search for a DisplayName with different case" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> displayNameExisting2.toUpperCase, "fields" -> Seq("displayName"))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "id").asOpt[String] must beSome(identityExisting2)

      data.length must beEqualTo(1)
    }

    "Find nothing for non-existing DisplayNames" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> "moeps", "fields" -> Seq("displayName"))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(0)
    }

    "Search for DisplayName or CameoId 1 " in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> displayNameExisting2.substring(2, 6), "fields" -> Seq("displayName", "cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "id").asOpt[String] must beSome(identityExisting2)

      data.length must beEqualTo(1)
    }

    "Search for DisplayName or CameoId 2 " in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> cameoIdExisting2.substring(0, 4), "fields" -> Seq("cameoId", "displayName"))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      (data(0) \ "cameoId").asOpt[String] must beSome(cameoIdExisting2)
      (data(0) \ "id").asOpt[String] must beSome(identityExisting2)

      data.length must beEqualTo(1)
    }

    "Don't return self on search for cameoId" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> cameoIdExisting, "fields" -> Seq("cameoId"))

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(0)
    }

    "Exclude contacts from search on demand" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> internalContact2CameoId, "fields" -> Seq("cameoId"), "excludeContacts" -> true)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(0)
    }

    "Include contacts to search on demand" in {

      val path = basePath + "/identity/search"

      val json = Json.obj("search" -> internalContact2CameoId, "fields" -> Seq("cameoId"), "excludeContacts" -> false)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beGreaterThan(0)
      data.find(js => (js \ "id").as[String].equals(internalContact2IdentityId)) must beSome
    }

    "should not find external contacts" in {

      val path = basePath + "/identity/search"
      val json = Json.obj("search" -> externalContactDisplayName, "fields" -> Seq("displayName"))
      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must equalTo(0)
    }

    "add public key to identity" in {
      val path = basePath + "/identity/publicKey"

      val json = Json.obj("name" -> pubKeyName, "key" -> pubKey, "keySize" -> pubKeySize)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
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

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
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

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "publicKeys").asOpt[Seq[JsObject]] must beSome
      val pubKeys = (data \ "publicKeys").as[Seq[JsObject]]

      pubKeys.length must beGreaterThanOrEqualTo(2)

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

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "check if public key was edited" in {
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "publicKeys").asOpt[Seq[JsObject]] must beSome
      val pubKeys = (data \ "publicKeys").as[Seq[JsObject]]

      pubKeys.length must beGreaterThanOrEqualTo(2)

      val key1: JsObject = pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId)).get
      (key1 \ "id").asOpt[String] must beSome(pubKeyId)
      (key1 \ "name").asOpt[String] must beSome(newPubKeyName)
      (key1 \ "key").asOpt[String] must beSome(pubKey)
      (key1 \ "keySize").asOpt[Int] must beSome(pubKeySize)

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

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "check if key was deleted" in {
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "publicKeys").asOpt[Seq[JsObject]] must beSome
      val pubKeys = (data \ "publicKeys").as[Seq[JsObject]]

      pubKeys.length must beGreaterThanOrEqualTo(1)

      pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId)) must beNone

      val key2: JsObject = pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId2)).get
      (key2 \ "id").asOpt[String] must beSome(pubKeyId2)
      (key2 \ "name").asOpt[String] must beSome(pubKeyName2)
      (key2 \ "key").asOpt[String] must beSome(pubKey2)
      (key2 \ "keySize").asOpt[Int] must beSome(pubKeySize2)
    }

    val fromKeyId = "fromMoep"
    val toKeyId = "toMoep"
    val encryptedTransactionSecret = "encryptedMoep"
    val signature = "singedByMoep"
    var authenticationRequestId = ""

    "add authenticationRequest" in {
      val path = basePath + "/identity/authenticationRequest"
      val json = Json.obj("fromKeyId" -> fromKeyId, "toKeyId" -> toKeyId, "encryptedTransactionSecret" -> encryptedTransactionSecret, "signature" -> signature)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "id").asOpt[String] must beSome
      authenticationRequestId = (data \ "id").as[String]
      (data \ "fromKeyId").asOpt[String] must beSome(fromKeyId)
      (data \ "toKeyId").asOpt[String] must beSome(toKeyId)
      (data \ "signature").asOpt[String] must beSome(signature)
      (data \ "encryptedTransactionSecret").asOpt[String] must beSome(encryptedTransactionSecret)
      (data \ "created").asOpt[Int] must beSome
    }

    "the new authentication request should be returned with get identity" in {
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "authenticationRequests").asOpt[Seq[JsObject]] must beSome
      val requests = (data \ "authenticationRequests").as[Seq[JsObject]]
      requests.length must beEqualTo(1)
      (requests(0) \ "id").asOpt[String] must beSome
      (requests(0) \ "fromKeyId").asOpt[String] must beSome(fromKeyId)
      (requests(0) \ "toKeyId").asOpt[String] must beSome(toKeyId)
      (requests(0) \ "signature").asOpt[String] must beSome(signature)
      (requests(0) \ "encryptedTransactionSecret").asOpt[String] must beSome(encryptedTransactionSecret)
      (requests(0) \ "created").asOpt[Int] must beSome
    }

    "delete authentication request" in {
      val path = basePath + "/identity/authenticationRequest/" + authenticationRequestId

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "deleted authentication request should not be there when getting identity" in {
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "authenticationRequests").asOpt[Seq[JsObject]] must beSome
      val requests = (data \ "authenticationRequests").as[Seq[JsObject]]
      requests.length must beEqualTo(0)
    }
  }
}
