package models

import play.api.libs.json._
import play.api.libs.functional.syntax._
import helper.IdHelper
import traits.SubModel
import scala.concurrent.{ Future, ExecutionContext }
import ExecutionContext.Implicits.global
import play.api.mvc.SimpleResult
import helper.ResultHelper._
import helper.JsonHelper._
import constants.Contacts._

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 5:53 PM
 */
case class Contact(id: MongoId,
                   groups: Seq[String],
                   identityId: MongoId,
                   docVersion: Int) {

  def toJson: JsObject = Json.toJson(this)(Contact.outputWrites).as[JsObject]

  def toJsonWithIdentity: Future[JsObject] =
    Identity.find(this.identityId).map {
      case None => Json.obj()
      case Some(identity) =>
        val contactType = identity.accountId match {
          case None    => CONTACT_TYPE_EXTERNAL
          case Some(a) => CONTACT_TYPE_INTERNAL
        }

        val identityJson = identity.accountId match {
          case None    => identity.toPrivateJson
          case Some(a) => identity.toPublicJson
        }

        Json.toJson(this)(Contact.outputWrites).as[JsObject] ++
          Json.obj("identity" -> identityJson) ++
          Json.obj("contactType" -> contactType)
    }

  def toJsonWithIdentityResult: Future[SimpleResult] = {
    this.toJsonWithIdentity.map(
      js => resOK(js))
  }

  def update(contactUpdate: ContactUpdate): Future[Boolean] = {

    // edit groups
    val updatedGroups = contactUpdate.groups match {
      case Some(groups) =>
        val query = arrayQuery("contacts", this.id)
        val set = Json.obj("$set" -> Json.obj("contacts.$.groups" -> groups))
        Contact.col.update(query, set).map(_.updatedExisting)
      case None => Future(false)
    }

    Identity.find(this.identityId).flatMap {
      case None => updatedGroups
      case Some(identity) =>
        // only update, if the identity does not have an account
        identity.accountId match {
          case Some(a) => updatedGroups
          case None =>
            val identityUpdate = new IdentityUpdate(VerifiedString.createOpt(contactUpdate.phoneNumber), VerifiedString.createOpt(contactUpdate.email), contactUpdate.displayName)
            identity.update(identityUpdate)
        }
    }
  }

}

object Contact extends SubModel[Contact, Identity] {

  def parentModel = Identity
  def elementName = "contacts"

  implicit val mongoFormat: Format[Contact] = createMongoFormat(Json.reads[Contact], Json.writes[Contact])

  def createReads(identityId: MongoId, contactType: String): Reads[Contact] = (
    Reads.pure[MongoId](IdHelper.generateContactId()) and
    ((__ \ 'groups).read[Seq[String]] or Reads.pure(Seq[String]())) and
    Reads.pure[MongoId](identityId) and
    Reads.pure[Int](docVersion)
  )(Contact.apply _)

  def outputWrites: Writes[Contact] = Writes {
    c =>
      Json.obj("id" -> c.id.toJson) ++
        Json.obj("groups" -> c.groups) ++
        Json.obj("identityId" -> c.identityId.toJson)
  }

  def create(identityId: MongoId, groups: Seq[String] = Seq(), id: Option[MongoId] = None): Contact = {
    val contactId = id match {
      case None      => IdHelper.generateContactId()
      case Some(cid) => cid
    }
    new Contact(contactId, groups, identityId, docVersion)
  }

  /*
   * Evolutions
   */

  val evolutionAddContactType: Reads[JsObject] = Reads[JsObject] {
    js =>
      val addType = __.json.update((__ \ 'contactType).json.put(JsString(CONTACT_TYPE_INTERNAL)))
      val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(1)))
      js.transform(addType andThen addVersion)
  }

  val docVersion = 1
  val evolutions = Map(0 -> evolutionAddContactType)

  override def createDefault(): Contact = {
    new Contact(IdHelper.generateContactId(), Seq(), IdHelper.generateMongoId(), docVersion)
  }

}

case class ContactUpdate(groups: Option[Seq[String]],
                         displayName: Option[String],
                         email: Option[String],
                         phoneNumber: Option[String])

object ContactUpdate {
  implicit val format = Json.format[ContactUpdate]
}