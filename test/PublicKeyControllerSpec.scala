import org.specs2.matcher.ValueCheck
import testHelper.StartedApp
import play.api.Logger
import play.api.test._
import play.api.libs.json.{ JsArray, JsString, Json, JsObject }
import play.api.test.Helpers._
import testHelper.Helper._
import org.specs2.mutable._
import testHelper.TestConfig._
import testHelper.StartedApp

/**
 * User: Björn Reimer
 * Date: 22.07.14
 * Time: 17:10
 */
class PublicKeyControllerSpec extends StartedApp {

  sequential

  val pubKey = "asdfasdfasdf"
  val pubKeyName = "moep"
  val pubKeyId = "Qhx213Vjr6GRSEawEL0WTzlb00whAuXpngy5zxc8HYc"
  val pubKeySize = 15
  val newPubKeyName = "poem"

  val pubKey2 = "asdfasdfasdf2"
  val pubKeyName2 = "moep2"
  val pubKeyId2 = "lwVCPOvBydpuXjTm2AvRxi2OWMG56UzPpwzkm5MSXFU"
  val pubKeySize2 = 2048

  "Public Key Controller should" in {

    "add public key to identity" in {
      val path = basePath + "/publicKey"

      val json = Json.obj("name" -> (pubKeyName + "foo"), "key" -> pubKey, "keySize" -> pubKeySize)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(pubKeyId)
      (data \ "name").asOpt[String] must beSome(pubKeyName + "foo")
      (data \ "key").asOpt[String] must beSome(pubKey)
      (data \ "keySize").asOpt[Int] must beSome(pubKeySize)
      (data \ "signatures").asOpt[Seq[JsObject]] must beSome
    }

    "add the same key a second time" in {
      val path = basePath + "/publicKey"

      val json = Json.obj("name" -> pubKeyName, "key" -> pubKey, "keySize" -> pubKeySize)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(pubKeyId)
      (data \ "name").asOpt[String] must beSome(pubKeyName)
      (data \ "key").asOpt[String] must beSome(pubKey)
      (data \ "keySize").asOpt[Int] must beSome(pubKeySize)
      (data \ "signatures").asOpt[Seq[JsObject]] must beSome
    }

    "refuse to add the same key to another identity" in {
      val path = basePath + "/publicKey"

      val json = Json.obj("name" -> pubKeyName, "key" -> pubKey, "keySize" -> pubKeySize)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != BAD_REQUEST) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(BAD_REQUEST)
    }

    "add another public key to identity" in {
      val path = basePath + "/publicKey"

      val json = Json.obj("name" -> pubKeyName2, "key" -> pubKey2, "keySize" -> pubKeySize2)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(pubKeyId2)
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
      val path = basePath + "/publicKey/" + pubKeyId + "/signature"

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
      val path = basePath + "/publicKey/" + pubKeyId + "/signature"

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
      val path = basePath + "/publicKey/" + pubKeyId + "/signature"

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
      val path = basePath + "/publicKey/" + pubKeyId + "/signature/" + signatureKeyId2

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

      val path = basePath + "/publicKey/" + pubKeyId

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
      val path = basePath + "/publicKey/" + pubKeyId

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

    val keyId = "moepsKeyId"
    val newKeyId = "newMoep"
    val aePassphrases = Seq("moepsPhrase1", "moepsPhrase2", "moepsPhrase3")
    val newAePassphrases = Seq("moepsPhrase1new", "moepsPhrase2new", "moepsPhrase3new")
    val conversationIds = Seq(cidExisting2, cidExisting3, cidExisting4)

    "add same the key again" in {
      val path = basePath + "/publicKey"

      val json = Json.obj("name" -> pubKeyName, "key" -> pubKey, "keySize" -> pubKeySize)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(pubKeyId)
      (data \ "name").asOpt[String] must beSome(pubKeyName)
      (data \ "key").asOpt[String] must beSome(pubKey)
      (data \ "keySize").asOpt[Int] must beSome(pubKeySize)
      (data \ "signatures").asOpt[Seq[JsObject]] must beSome
    }

    "the key should be restored with the old signatures" in {
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

    "add AePassphrase to conversation 1" in {
      val path = basePath + "/conversation/" + conversationIds(0) + "/aePassphrases"

      val json = Json.obj("aePassphraseList" -> Seq(Json.obj("keyId" -> keyId, "encryptedPassphrase" -> aePassphrases(0))))

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "add AePassphrase to conversation 2" in {
      val path = basePath + "/conversation/" + conversationIds(1)  + "/aePassphrases"

      val json = Json.obj("aePassphraseList" -> Seq(Json.obj("keyId" -> keyId, "encryptedPassphrase" -> aePassphrases(1))))

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "add AePassphrase to conversation 3" in {
      val path = basePath + "/conversation/" + conversationIds(2) + "/aePassphrases"

      val json = Json.obj("aePassphraseList" -> Seq(Json.obj("keyId" -> keyId, "encryptedPassphrase" -> aePassphrases(2))))

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "return all aePassphrases with conversationIds for rekeying" in {
      val path = basePath + "/publicKey/" + keyId + "/aePassphrases?newKeyId=" + newKeyId
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
      val path = basePath + "/publicKey/" + keyId + "/aePassphrases?newKeyId=" + newKeyId + "&limit=2"
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
      val path = basePath + "/conversation/" + conversationIds(0)   + "/aePassphrases"

      val json = Json.obj("aePassphraseList" -> Seq(Json.obj("keyId" -> newKeyId, "encryptedPassphrase" -> newAePassphrases(0))))

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "do not return conversation 1 when asking for aePassphrases for rekeying" in {

      val path = basePath + "/publicKey/" + keyId + "/aePassphrases?newKeyId=" + newKeyId
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
      val path = basePath + "/publicKey/" + newKeyId + "/aePassphrases"

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

      val path = basePath + "/publicKey/" + keyId + "/aePassphrases?newKeyId=" + newKeyId
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

    val testUser = TestUser.create()
    var newPubKeyId = ""
    val keyId1 = "moep1"
    val keyId2 = "moep2"
    val keyId3 = "moep3"

    "add public key other identity" in {
      val path = basePath + "/publicKey"

      val json = Json.obj("name" -> "mamama", "key" -> "kkkkeeeeyyyy", "keySize" -> 12)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser.token)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      newPubKeyId = (data \ "id").as[String]
      1 === 1

    }

    "add 2 signatures to that public key" in {
      Seq(keyId1, keyId2).map { id =>
        val path = basePath + "/publicKey/" + newPubKeyId + "/signature"

        val json = Json.obj("keyId" -> id, "content" -> "mmooeepp")

        val req = FakeRequest(POST, path).withHeaders(tokenHeader(testUser.token)).withJsonBody(json)
        val res = route(req).get

        if (status(res) != OK) {
          Logger.error("Response: " + contentAsString(res))
        }
        status(res) must equalTo(OK)
      }
      1 === 1
    }

    "get public key with signatures of that other identity" in {
      val path = basePath + "/identity/" + testUser.identityId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      val pubKeys = (data \ "publicKeys").as[Seq[JsObject]]
      (pubKeys(0) \ "signatures").asOpt[Seq[JsObject]] must beSome
      val signatures = (pubKeys(0) \ "signatures").as[Seq[JsObject]]
      signatures must haveLength(2)
      signatures.find(js => (js \ "keyId").as[String].equals(keyId1)) must beSome
      signatures.find(js => (js \ "keyId").as[String].equals(keyId2)) must beSome
    }

    "add own signature to that public key" in {
      val path = basePath + "/publicKey/" + newPubKeyId + "/signature"

      val json = Json.obj("keyId" -> keyId3, "content" -> "mmooeepp")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)
    }

    "other identity should now contain the signature" in {
      val path = basePath + "/identity/" + testUser.identityId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      val pubKeys = (data \ "publicKeys").as[Seq[JsObject]]
      (pubKeys(0) \ "signatures").asOpt[Seq[JsObject]] must beSome
      val signatures = (pubKeys(0) \ "signatures").as[Seq[JsObject]]
      signatures must haveLength(3)
      signatures.find(js => (js \ "keyId").as[String].equals(keyId1)) must beSome
      signatures.find(js => (js \ "keyId").as[String].equals(keyId2)) must beSome
      signatures.find(js => (js \ "keyId").as[String].equals(keyId3)) must beSome
    }

    "the signature should not be returned if another identity gets it" in {
      val path = basePath + "/identity/" + testUser.identityId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      if (status(res) != OK) {
        Logger.error("Response: " + contentAsString(res))
      }
      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]
      val pubKeys = (data \ "publicKeys").as[Seq[JsObject]]
      (pubKeys(0) \ "signatures").asOpt[Seq[JsObject]] must beSome
      val signatures = (pubKeys(0) \ "signatures").as[Seq[JsObject]]
      signatures must haveLength(2)
      signatures.find(js => (js \ "keyId").as[String].equals(keyId1)) must beSome
      signatures.find(js => (js \ "keyId").as[String].equals(keyId2)) must beSome
      signatures.find(js => (js \ "keyId").as[String].equals(keyId3)) must beNone
    }
  }
}