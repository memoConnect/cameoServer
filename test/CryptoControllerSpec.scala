import testHelper.StartedApp
import play.api.Logger
import play.api.test._
import play.api.libs.json.{ JsArray, JsString, Json, JsObject }
import play.api.test.Helpers._
import testHelper.Stuff._
import org.specs2.mutable._
import testHelper.TestConfig._
import testHelper.StartedApp

/**
 * User: Björn Reimer
 * Date: 22.07.14
 * Time: 17:10
 */
class CryptoControllerSpec extends StartedApp {

  sequential

  val pubKey = "asdfasdfasdf"
  val pubKeyName = "moep"
  var pubKeyId = ""
  val pubKeySize = 15
  val newPubKeyName = "poem"

  val pubKey2 = "asdfasdfasdf2"
  val pubKeyName2 = "moep2"
  var pubKeyId2 = ""
  val pubKeySize2 = 2048

  "Crypto Controller should" in {
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
      (data \ "signatures").asOpt[Seq[JsObject]] must beSome
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
      (data \ "signatures").asOpt[Seq[JsObject]] must beSome

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
      (key1 \ "signatures").asOpt[Seq[JsObject]] must beSome

      val key2: JsObject = pubKeys.find(js => (js \ "id").as[String].equals(pubKeyId2)).get
      (key2 \ "id").asOpt[String] must beSome(pubKeyId2)
      (key2 \ "name").asOpt[String] must beSome(pubKeyName2)
      (key2 \ "key").asOpt[String] must beSome(pubKey2)
      (key2 \ "keySize").asOpt[Int] must beSome(pubKeySize2)
      (key2 \ "signatures").asOpt[Seq[JsObject]] must beSome

    }

    val signature = "moepsSignature"
    val signature2 = "moepsSignature2"
    val signature3 = "moepsSignature3"
    val signatureKeyId = "moepKeyId"
    val signatureKeyId2 = "moepKeyId2"

    "add signature to public key" in {
      val path = basePath + "/identity/publicKey/" + pubKeyId + "/signature"

      val json = Json.obj("keyId" -> signatureKeyId, "content" -> signature)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "keyId").asOpt[String] must beSome(signatureKeyId)
      (data \ "content").asOpt[String] must beSome(signature)
    }

    "add another signature to public key" in {
      val path = basePath + "/identity/publicKey/" + pubKeyId + "/signature"

      val json = Json.obj("keyId" -> signatureKeyId2, "content" -> signature3)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "keyId").asOpt[String] must beSome(signatureKeyId2)
      (data \ "content").asOpt[String] must beSome(signature3)
    }

    "overwrite the second signature" in {
      val path = basePath + "/identity/publicKey/" + pubKeyId + "/signature"

      val json = Json.obj("keyId" -> signatureKeyId2, "content" -> signature2)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      (data \ "keyId").asOpt[String] must beSome(signatureKeyId2)
      (data \ "content").asOpt[String] must beSome(signature2)
    }

    "both signatures should be returned with public key" in {

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
      (key1 \ "signatures").asOpt[Seq[JsObject]] must beSome

      val signatures = (key1 \ "signatures").as[Seq[JsObject]]
      signatures.length must beEqualTo(2)

      val sig1: JsObject = signatures.find(js => (js \ "keyId").as[String].equals(signatureKeyId)).get
      (sig1 \ "content").asOpt[String] must beSome(signature)

      val sig2: JsObject = signatures.find(js => (js \ "keyId").as[String].equals(signatureKeyId2)).get
      (sig2 \ "content").asOpt[String] must beSome(signature2)
    }

    "delete signature" in {
      val path = basePath + "/identity/publicKey/" + pubKeyId + "/signature/" + signatureKeyId2

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "deleted signatures should not be returned anymore" in {

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
      (key1 \ "signatures").asOpt[Seq[JsObject]] must beSome

      val signatures = (key1 \ "signatures").as[Seq[JsObject]]
      signatures.length must beEqualTo(1)

      val sig1: JsObject = signatures.find(js => (js \ "keyId").as[String].equals(signatureKeyId)).get
      (sig1 \ "content").asOpt[String] must beSome(signature)
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
    val authSignature = "singedByMoep"
    var authenticationRequestId = ""
    val encryptedTransactionSecret2 = "encryptedMoep2"
    val authSignature2 = "singedByMoep2"
    var authenticationRequestId2 = ""

    "add authenticationRequest" in {
      val path = basePath + "/identity/authenticationRequest"
      val json = Json.obj("fromKeyId" -> fromKeyId, "toKeyId" -> toKeyId, "encryptedTransactionSecret" -> encryptedTransactionSecret, "signature" -> authSignature)

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
      (data \ "signature").asOpt[String] must beSome(authSignature)
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
      (requests(0) \ "signature").asOpt[String] must beSome(authSignature)
      (requests(0) \ "encryptedTransactionSecret").asOpt[String] must beSome(encryptedTransactionSecret)
      (requests(0) \ "created").asOpt[Int] must beSome
    }

    "add another authentication request with the same keyIds" in {
      val path = basePath + "/identity/authenticationRequest"
      val json = Json.obj("fromKeyId" -> fromKeyId, "toKeyId" -> toKeyId, "encryptedTransactionSecret" -> encryptedTransactionSecret2, "signature" -> authSignature2)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      authenticationRequestId2 = (data \ "id").as[String]

      1 === 1
    }

    "the new authentication request should have replaced the old one" in {
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
      (requests(0) \ "id").asOpt[String] must beSome(authenticationRequestId2)
      (requests(0) \ "fromKeyId").asOpt[String] must beSome(fromKeyId)
      (requests(0) \ "toKeyId").asOpt[String] must beSome(toKeyId)
      (requests(0) \ "signature").asOpt[String] must beSome(authSignature2)
      (requests(0) \ "encryptedTransactionSecret").asOpt[String] must beSome(encryptedTransactionSecret2)
      (requests(0) \ "created").asOpt[Int] must beSome
    }

    "delete authentication request" in {
      val path = basePath + "/identity/authenticationRequest/" + authenticationRequestId2

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "return error when trying to delete the same request again" in {
      val path = basePath + "/identity/authenticationRequest/" + authenticationRequestId2

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != NOT_FOUND) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(NOT_FOUND)
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

    val keyId = "moepsKeyId"
    val newKeyId = "newMoep"
    val aePassphrases = Seq("moepsPhrase1", "moepsPhrase2", "moepsPhrase3")
    val newAePassphrases = Seq("moepsPhrase1new", "moepsPhrase2new", "moepsPhrase3new")
    val conversationIds = Seq(cidExisting2, cidExisting3, cidExisting4)

    "add AePassphrase to conversation 1" in {
      val path = basePath + "/conversation/" + conversationIds(0)

      val json = Json.obj("aePassphraseList" -> Seq(Json.obj("keyId" -> keyId, "encryptedPassphrase" -> aePassphrases(0))))

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "add AePassphrase to conversation 2" in {
      val path = basePath + "/conversation/" + conversationIds(1)

      val json = Json.obj("aePassphraseList" -> Seq(Json.obj("keyId" -> keyId, "encryptedPassphrase" -> aePassphrases(1))))

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "add AePassphrase to conversation 3" in {
      val path = basePath + "/conversation/" + conversationIds(2)

      val json = Json.obj("aePassphraseList" -> Seq(Json.obj("keyId" -> keyId, "encryptedPassphrase" -> aePassphrases(2))))

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "return all aePassphrases with conversationIds for rekeying" in {
      val path = basePath + "/identity/publicKey/" + keyId + "/aePassphrases?newKeyId=" + newKeyId
      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(3)

      data.map {
        aep =>
          (aep \ "conversationId").asOpt[String] must beSome
          (aep \ "aePassphrase").asOpt[String] must beSome
      }

      aePassphrases.zip(conversationIds).foreach {
        case (aep, cid) =>
          val res = data.find(js => (js \ "conversationId").as[String].equals(cid))
          res must beSome
          (res.get \ "aePassphrase").asOpt[String] must beSome(aep)
      }
      1 === 1
    }

    "apply limit to result" in {
      val path = basePath + "/identity/publicKey/" + keyId + "/aePassphrases?newKeyId=" + newKeyId + "&limit=2"
      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(2)
    }

    "add aePassphrase for new key to conversation 1" in {
      val path = basePath + "/conversation/" + conversationIds(0)

      val json = Json.obj("aePassphraseList" -> Seq(Json.obj("keyId" -> newKeyId, "encryptedPassphrase" -> newAePassphrases(0))))

      val req = FakeRequest(PUT, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "do not return conversation 1 when asking for aePassphrases for rekeying" in {

      val path = basePath + "/identity/publicKey/" + keyId + "/aePassphrases?newKeyId=" + newKeyId
      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(2)

      data.map {
        aep =>
          (aep \ "conversationId").asOpt[String] must beSome
          (aep \ "aePassphrase").asOpt[String] must beSome
      }

      aePassphrases.zip(conversationIds).drop(1).foreach {
        case (aep, cid) =>
          val res = data.find(js => (js \ "conversationId").as[String].equals(cid))
          res must beSome
          (res.get \ "aePassphrase").asOpt[String] must beSome(aep)
      }
      1 === 1
    }

    "add new aePassphrase to the other two conversation" in {
      val path = basePath + "/identity/publicKey/" + newKeyId + "/aePassphrases"

      val list = newAePassphrases.zip(conversationIds).drop(1).map {
        case (aep, cid) => Json.obj("conversationId" -> cid, "aePassphrase" -> aep)
      }

      val json = JsArray(list)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "no more aePassphrases should be returned" in {

      val path = basePath + "/identity/publicKey/" + keyId + "/aePassphrases?newKeyId=" + newKeyId
      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(0)
    }

    "verify that new aePassphrase was added to conversation 2" in {

      val path = basePath + "/conversation/" + conversationIds(1) + "?keyId=" + newKeyId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "aePassphraseList").asOpt[Seq[JsObject]] must beSome
      val encPasses = (data \ "aePassphraseList").as[Seq[JsObject]]

      encPasses.length must beEqualTo(1)

      (encPasses(0) \ "keyId").asOpt[String] must beSome(newKeyId)
      (encPasses(0) \ "encryptedPassphrase").asOpt[String] must beSome(newAePassphrases(1))
    }
  }
}
