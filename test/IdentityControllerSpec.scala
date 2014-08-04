import play.api.Play.current
import play.api.{Play, Logger}
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
      (data \ "displayName").asOpt[String] must beSome
      (data \ "avatar").asOpt[String] must beSome
      (data \ "publicKeys").asOpt[Seq[JsObject]] must beSome
    }

    "Get internal identity without token" in {
      val path = basePath + "/identity/" + identityExisting

      val req = FakeRequest(GET, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "userKey").asOpt[String] must beNone
      (data \ "cameoId").asOpt[String] must beSome
      (data \ "email" \ "value").asOpt[String] must beSome(emailExisting)
      (data \ "phoneNumber" \ "value").asOpt[String] must beSome(telExisting)
      (data \ "displayName").asOpt[String] must beSome
      (data \ "avatar").asOpt[String] must beSome
      (data \ "publicKeys").asOpt[Seq[JsObject]] must beSome
    }

    "Get external identity without token" in {
      val path = basePath + "/identity/" + externalContact2IdentityId

      val req = FakeRequest(GET, path)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "userKey").asOpt[String] must beNone
      (data \ "cameoId").asOpt[String] must beSome
      (data \ "email" \ "value").asOpt[String] must beNone
      (data \ "phoneNumber" \ "value").asOpt[String] must beNone
      (data \ "displayName").asOpt[String] must beSome
      (data \ "avatar").asOpt[String] must beSome
      (data \ "publicKeys").asOpt[Seq[JsObject]] must beSome
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

      (data(0) \ "cameoId").asOpt[String] must beSome(cameoIdExisting2 + "@" + domain)
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

      (data(0) \ "cameoId").asOpt[String] must beSome(cameoIdExisting2 + "@" + domain)
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

      (data(0) \ "cameoId").asOpt[String] must beSome(cameoIdExisting2 + "@" + domain)
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

      (data(0) \ "cameoId").asOpt[String] must beSome(cameoIdExisting2 + "@" + domain)
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

    "send friend request" in {
      val path = basePath + "/friendRequest"

      val json = Json.obj("identityId" -> identityExisting4)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

    }

    "pending friend requests should be excluded in search" in {

      val path = basePath + "/identity/search"
      val json = Json.obj("search" -> cameoIdExisting4, "fields" -> Seq("cameoId"))
      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must equalTo(0)
    }

    "reject pending friend request" in {
      val path = basePath + "/friendRequest/answer"
      val json = Json.obj("answerType" -> "reject", "identityId" -> identityExisting)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting4)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "identity should be returned again in search" in {
      val path = basePath + "/identity/search"
      val json = Json.obj("search" -> cameoIdExisting4, "fields" -> Seq("cameoId"))
      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data: Seq[JsObject] = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must equalTo(1)

      (data(0) \ "cameoId").asOpt[String] must beSome(cameoIdExisting4 + "@" + domain)
      (data(0) \ "id").asOpt[String] must beSome(identityExisting4)
    }

    val testUser = createTestUser()
    val newIdentityDisplayName = "Mooeepp"
    val newIdentityTel = "+49123456"
    val newIdentityEmail = "foo@moep.de"
    val newIdentityCameoId = "myMoep"
    var newIdentityId = ""

    "refuse to add new identity to account without reservation secret" in {
      val path = basePath + "/identity"
      val json = Json.obj("displayName" -> newIdentityDisplayName, "phoneNumber" -> newIdentityTel, "email" -> newIdentityEmail, "cameoId" -> newIdentityCameoId)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
    }

    "refuse to add new identity to account with invalid reservation secret" in {
      val path = basePath + "/identity"
      val json = Json.obj("displayName" -> newIdentityDisplayName, "phoneNumber" -> newIdentityTel, "email" -> newIdentityEmail, "cameoId" -> newIdentityCameoId, "reservationSecret" -> "fooooo")

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
    }

    var regSec = ""
    "reserve cameoId" in {
      val path = basePath + "/account/check"
      val json = Json.obj("cameoId" -> newIdentityCameoId)

      val req = FakeRequest(POST, path).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "reservationSecret").asOpt[String] must beSome
      regSec = (data \ "reservationSecret").as[String]
      1 === 1
    }

    "add new identity to account with correct reservation secret" in {
      val path = basePath + "/identity"
      val json = Json.obj("displayName" -> newIdentityDisplayName, "phoneNumber" -> newIdentityTel, "email" -> newIdentityEmail, "cameoId" -> newIdentityCameoId, "reservationSecret" -> regSec)

      val req = FakeRequest(POST, path).withJsonBody(json).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      newIdentityId = (data \ "id").as[String]
      (data \ "userKey").asOpt[String] must beSome
      (data \ "cameoId").asOpt[String] must beSome(newIdentityCameoId + "@" + domain)
      (data \ "email" \ "value").asOpt[String] must beSome(newIdentityEmail)
      (data \ "phoneNumber" \ "value").asOpt[String] must beSome(newIdentityTel)
      (data \ "displayName").asOpt[String] must beSome(newIdentityDisplayName)
      (data \ "avatar").asOpt[String] must beSome
      (data \ "publicKeys").asOpt[Seq[JsObject]] must beSome
    }

    "account should now contain both identities" in {
      val path = basePath + "/account"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "identities").asOpt[Seq[JsObject]] must beSome
      val identities = (data \ "identities").as[Seq[JsObject]]
      identities.length must beEqualTo(2)

      identities.find(js => (js \ "id").as[String].equals(testUser.identityId)) must beSome
      identities.find(js => (js \ "id").as[String].equals(newIdentityId)) must beSome

      val identity = identities.find(js => (js \ "id").as[String].equals(newIdentityId)).get
      (identity \ "id").asOpt[String] must beSome
      (identity \ "userKey").asOpt[String] must beSome
      (identity \ "cameoId").asOpt[String] must beSome(newIdentityCameoId + "@" + domain)
      (identity \ "email" \ "value").asOpt[String] must beSome(newIdentityEmail)
      (identity \ "phoneNumber" \ "value").asOpt[String] must beSome(newIdentityTel)
      (identity \ "displayName").asOpt[String] must beSome(newIdentityDisplayName)
      (identity \ "avatar").asOpt[String] must beSome
      (identity \ "publicKeys").asOpt[Seq[JsObject]] must beSome
    }

    var newToken = ""
    "get token for new identity" in {
      val path = basePath + "/identity/" + newIdentityId + "/token"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
      (contentAsJson(res) \ "data" \ "token").asOpt[String] must beSome
      newToken = (contentAsJson(res) \ "data" \ "token").as[String]

      1 === 1
    }

    "get identity with new token" in {
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(newToken))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      (data \ "userKey").asOpt[String] must beSome
      (data \ "cameoId").asOpt[String] must beSome(newIdentityCameoId + "@" + domain)
      (data \ "email" \ "value").asOpt[String] must beSome(newIdentityEmail)
      (data \ "phoneNumber" \ "value").asOpt[String] must beSome(newIdentityTel)
      (data \ "displayName").asOpt[String] must beSome(newIdentityDisplayName)
      (data \ "avatar").asOpt[String] must beSome
      (data \ "publicKeys").asOpt[Seq[JsObject]] must beSome
    }

    var fileId = ""
    "identity should have avatar" in {
      val path = basePath + "/identity"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(newToken))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "avatar").asOpt[String] must beSome
      fileId = (data \ "avatar").as[String]
      1 === 1
    }

    "check that avatar file meta exist" in {

      val path = basePath + "/file/" + fileId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(newToken))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(fileId)
      (data \ "chunks").asOpt[Seq[Int]] must beSome
      (data \ "chunks")(0).asOpt[Int] must beSome(0)
      (data \ "chunks")(1).asOpt[Int] must beNone
      (data \ "fileName").asOpt[String] must beSome("avatar.png")
      (data \ "fileSize").asOpt[Int] must beSome
      (data \ "fileType").asOpt[String] must beSome("image/png")
    }

    "check that avatar file chunk exists" in {
      val path = basePath + "/file/" + fileId + "/" + 0

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(newToken))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
      val raw = contentAsBytes(res)

      raw.length must beGreaterThan(100)
    }

    "identity should have support user as contact" in {
      val path = basePath + "/contacts"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(newToken))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(1)
      (data(0) \ "identityId").asOpt[String] must beEqualTo(Play.configuration.getString("support.contact.identityId"))
    }

    "refuse to return token for identity that is not in the same account" in {
      val path = basePath + "/identity/" + identityExisting + "/token"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != NOT_FOUND) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(NOT_FOUND)
    }

    "refuse to return token for external identity"in {
      val path = basePath + "/identity/" + externalContactIdentityId + "/token"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(testUser.token))
      val res = route(req).get

      if (status(res) != NOT_FOUND) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(NOT_FOUND)
    }

    var tokenOldIdentity = ""
    "return token for first identity when loggin in" in {
      val path = basePath + "/token"

      val auth = "Basic " + new sun.misc.BASE64Encoder().encode((testUser.login + ":" + password).getBytes)

      val req = FakeRequest(GET, path).withHeaders(("Authorization", auth))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      (contentAsJson(res) \ "data" \ "token").asOpt[String] must beSome
      tokenOldIdentity = (contentAsJson(res) \ "data" \ "token").as[String]

      1 === 1
    }

    "check that default identity is returned" in {
      val path = basePath + "/identity"
      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenOldIdentity))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "id").asOpt[String] must beSome(testUser.identityId)
    }
  }
}
