import play.api.libs.json.JsObject
import play.api.libs.json.{JsArray, Json, JsObject}
import play.api.test.{FakeRequest, FakeApplication}
import play.api.test.Helpers._
import scala.Some
import testHelper.Stuff._
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import play.api.Logger
import testHelper.{StartedApp, Stuff}
import org.specs2.mutable._
import testHelper.TestConfig._

/**
 * User: Björn Reimer
 * Date: 3/3/14
 * Time: 4:48 PM
 */
class ContactControllerSpec extends StartedApp {

  sequential

  var identityOf10thContact = ""
  var idOf10thContact = ""
  var internalContactId = ""
  var externalContactId = ""
  val newContactMail = "test@bjrm.de"
  val newContactTel = "+4561233"
  val newContactName = "foobar"


  "ContactController" should {


    "get all contacts" in {
      val path = basePath + "/contacts"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(44)

      val contact = data(10)
      (contact \ "groups")(0).asOpt[String] must beSome("group1")
      (contact \ "identityId").asOpt[String] must beSome
      identityOf10thContact = (contact \ "identityId").as[String]
      (contact \ "id").asOpt[String] must beSome
      idOf10thContact = (contact \ "id").as[String]
      (contact \ "identity" \ "displayName").asOpt[String] must beSome
    }

    "get all contacts with offset" in {

      val path = basePath + "/contacts?offset=10"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(34)
    }

    "get all contacts with limit" in {

      val path = basePath + "/contacts?limit=20"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(20)
    }

    "get all contacts with limit and offset" in {
      val path = basePath + "/contacts?offset=10&limit=20"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(20)
      (data(0) \ "identityId").asOpt[String] must beSome(identityOf10thContact)
    }

    "add internal contact" in {
      val path = basePath + "/contact"

      val json = Json.obj("groups" -> Seq("group3", "group1"), "identityId" -> identityExisting2)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      internalContactId = (data \ "id").as[String]
      (data \ "groups")(0).asOpt[String] must beSome("group3")
      (data \ "groups")(1).asOpt[String] must beSome("group1")
      (data \ "identityId").asOpt[String] must beSome(identityExisting2)
      (data \ "contactType").asOpt[String] must beSome("internal")
    }

    "refuse to add internal contact that already exists" in {
      val path = basePath + "/contact"

      val json = Json.obj("groups" -> Seq("group3", "group1"), "identityId" -> identityExisting2)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(232)


    }

    "refuse to add internal contact with invalid identity" in {
      val path = basePath + "/contact"

      val json = Json.obj("groups" -> Seq("group3", "group1"), "identityId" -> "asdf")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(NOT_FOUND)
    }

    "get the new internal contact" in {

      val path = basePath + "/contact/" + internalContactId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "identityId").asOpt[String] must beSome(identityExisting2)
    }

    "edit groups of internal contact" in {

      val path = basePath + "/contact/" + internalContactId

      val newGroups = Seq("group1", "group4")
      val json = Json.obj("groups" -> newGroups)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "refuse to edit mail of internal contact" in {
      val path = basePath + "/contact/" + internalContactId

      val newMail = "new@mail.de"
      val json = Json.obj("email" -> newMail)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "refuse to edit phoneNumber of internal contact" in {
      val path = basePath + "/contact/" + internalContactId

      val newPhone = "+142536"
      val json = Json.obj("phoneNumber" -> newPhone)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "refuse to edit DisplayName of internal contact" in {
      val path = basePath + "/contact/" + internalContactId

      val newName = "fail"
      val json = Json.obj("displayName" -> newName)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(UNAUTHORIZED)
    }

    "add external contact" in {
      val path = basePath + "/contact"

      val mail = "some@mail.com"
      val tel = "+123456789123"
      val name = "foo"
      val json = Json.obj("groups" -> Seq("group1", "group2"),
        "identity" -> Json.obj("email" -> mail, "phoneNumber" -> tel, "displayName" -> name))

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome
      externalContactId = (data \ "id").as[String]
      (data \ "groups")(0).asOpt[String] must beSome("group1")
      (data \ "groups")(1).asOpt[String] must beSome("group2")
      (data \ "contactType").asOpt[String] must beSome("external")
      (data \ "identity" \ "email" \ "value").asOpt[String] must beSome(mail)
      (data \ "identity" \ "phoneNumber" \ "value").asOpt[String] must beSome(tel)
      (data \ "identity" \ "displayName").asOpt[String] must beSome(name)

    }

    "get the new external contact" in {

      val path = basePath + "/contact/" + externalContactId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[JsObject]

      (data \ "id").asOpt[String] must beSome(externalContactId)
      (data \ "groups")(0).asOpt[String] must beSome("group1")
      (data \ "groups")(1).asOpt[String] must beSome("group2")
      (data \ "contactType").asOpt[String] must beSome("external")
      (data \ "identity" \ "email" \ "value").asOpt[String] must beSome
      (data \ "identity" \ "phoneNumber" \ "value").asOpt[String] must beSome
      (data \ "identity" \ "displayName").asOpt[String] must beSome
    }

    "edit groups of external contact" in {

      val path = basePath + "/contact/" + externalContactId

      val newGroups = Seq("group1", "group3")
      val json = Json.obj("groups" -> newGroups)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "edit details of new contact" in {
      val path = basePath + "/contact/" + externalContactId

      val json = Json.obj("email" -> newContactMail, "phoneNumber" -> newContactTel, "displayName" -> newContactName)

      val req = FakeRequest(PUT, path).withJsonBody(json).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "get all contact groups" in {
      val path = basePath + "/contact-groups"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[String]]

      data.find(_.equals("group1")) aka "contain group 1" must beSome
      data.find(_.equals("group2")) aka "contain group 2" must beSome
      data.find(_.equals("group3")) aka "contain group 3" must beSome
      data.find(_.equals("group4")) aka "contain group 4" must beSome
    }

    "get single group" in {
      val path = basePath + "/contact-group/group1"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(24)

    }

    "get group with created internal contact" in {
      val path = basePath + "/contact-group/group4"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(1)

      (data(0) \ "identityId").asOpt[String] must beSome(identityExisting2)
    }

    "get group with created external contact" in {
      val path = basePath + "/contact-group/group3"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(1)

      (data(0) \ "id").asOpt[String] must beSome(externalContactId)
      (data(0) \ "groups")(0).asOpt[String] must beSome("group1")
      (data(0) \ "groups")(1).asOpt[String] must beSome("group3")
      (data(0) \ "contactType").asOpt[String] must beSome("external")
      (data(0) \ "identity" \ "email" \ "value").asOpt[String] must beSome(newContactMail)
      (data(0) \ "identity" \ "phoneNumber" \ "value").asOpt[String] must beSome(newContactTel)
      (data(0) \ "identity" \ "displayName").asOpt[String] must beSome(newContactName)
    }

    "delete Contact" in {
      val path = basePath + "/contact/" + internalContactId

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "check deletion" in {
      val path = basePath + "/contact/" + internalContactId

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(NOT_FOUND)
    }

    "refuse to delete non-existing contact" in {
      val path = basePath + "/contact/asdfasdf"

      val req = FakeRequest(DELETE, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(NOT_FOUND)
    }

    "send FriendRequest with identityId" in {
      val path = basePath + "/friendRequest"

      val json = Json.obj("identityId" -> identityExisting2)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "refuse to send FriendRequest to invalid identityId" in {
      val path = basePath + "/friendRequest"

      val json = Json.obj("identityId" -> "asdfasdf")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(NOT_FOUND)
    }

//    "send FriendRequest with cameoId" in {
//      val path = basePath + "/friendRequest"
//
//      val json = Json.obj("cameoId" -> login)
//
//      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
//      val res = route(req).get
//
//      status(res) must equalTo(OK)
//    }

    "refuse to send FriendRequest to invalid cameoId" in {
      val path = basePath + "/friendRequest"

      val json = Json.obj("cameoId" -> "pups")

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(NOT_FOUND)
    }

    "send another FriendRequest" in {
      val path = basePath + "/friendRequest"

      val json = Json.obj("identityId" -> identityExisting2)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "get friendRequest and check that there is only one" in {
      val path = basePath + "/friendRequests"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(1)

      (data(0) \ "id").asOpt[String] must beSome(identityExisting)
    }

    "reject FriendRequest" in {

      val path = basePath + "/friendRequest/answer"
      val json = Json.obj("answerType" -> "reject", "identityId" -> identityExisting)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "check if friendRequest is gone" in {
      val path = basePath + "/friendRequests"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(0)
    }

    "send another FriendRequest" in {
      val path = basePath + "/friendRequest"

      val json = Json.obj("identityId" -> identityExisting2)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "accept FriendRequest" in {
      val path = basePath + "/friendRequest/answer"

      val json = Json.obj("answerType" -> "accept", "identityId" -> identityExisting)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting2)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(OK)
    }

    "check if contact was added to sender" in {
      val path = basePath + "/contacts"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.find(c => (c \ "identityId").as[String].equals(identityExisting)) must beSome

    }

    "check if contact was added to receiver" in {
      val path = basePath + "/contacts"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.find(c => (c \ "identityId").as[String].equals(identityExisting2)) must beSome
    }

    "check if friendRequest is gone" in {
      val path = basePath + "/friendRequests"

      val req = FakeRequest(GET, path).withHeaders(tokenHeader(tokenExisting2))
      val res = route(req).get

      status(res) must equalTo(OK)

      val data = (contentAsJson(res) \ "data").as[Seq[JsObject]]

      data.length must beEqualTo(0)
    }

    "refuse to send friend request to identity that is already in contacts" in {
      val path = basePath + "/friendRequest"

      val json = Json.obj("identityId" -> identityExisting2)

      val req = FakeRequest(POST, path).withHeaders(tokenHeader(tokenExisting)).withJsonBody(json)
      val res = route(req).get

      status(res) must equalTo(232)
    }

      }
}