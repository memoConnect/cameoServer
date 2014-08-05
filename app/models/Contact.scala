package models

import constants.Contacts._
import helper.IdHelper
import helper.ResultHelper._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.Result
import traits.SubModel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 5:53 PM
 */
case class Contact(id: MongoId,
                   groups: Seq[String],
                   identityId: MongoId,
                   signatures: Seq[Signature],
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

  def toJsonWithIdentityResult: Future[Result] = {
    this.toJsonWithIdentity.map(
      js => resOk(js))
  }

  def update(contactUpdate: ContactUpdate): Future[Boolean] = {

//    // edit groups
//    val updatedGroups = contactUpdate.groups match {
//      case Some(groups) =>
//        val query = Json.obj("contacts._id" -> this.id)
//        val set = Json.obj("$set" -> Json.obj("contacts.$.groups" -> groups))
//        Contact.col.update(query, set).map(_.updatedExisting)
//      case None => Future(false)
//    }

    Future(true)

  }

}

object Contact extends SubModel[Contact, Identity] {

  def parentModel = Identity
  def elementName = "contacts"

  implicit val mongoFormat: Format[Contact] = createMongoFormat(Json.reads[Contact], Json.writes[Contact])

  def createReads(identityId: MongoId): Reads[Contact] = (
    Reads.pure[MongoId](IdHelper.generateContactId()) and
    ((__ \ 'groups).read[Seq[String]] or Reads.pure(Seq[String]())) and
    Reads.pure[MongoId](identityId) and
    Reads.pure[Seq[Signature]](Seq()) and
    Reads.pure[Int](docVersion)
  )(Contact.apply _)

  def outputWrites: Writes[Contact] = Writes {
    c =>
      Json.obj("id" -> c.id.toJson) ++
        Json.obj("groups" -> c.groups) ++
        Json.obj("signatures" -> c.signatures) ++
        Json.obj("identityId" -> c.identityId.toJson)
  }

  def create(identityId: MongoId, groups: Seq[String] = Seq(), id: Option[MongoId] = None): Contact = {
    val contactId = id match {
      case None      => IdHelper.generateContactId()
      case Some(cid) => cid
    }
    new Contact(contactId, groups, identityId, Seq(), docVersion)
  }

  /*
   * Evolutions
   */

  val docVersion = 2
  val evolutions = Map(
    0 -> ContactEvolutions.addContactType,
    1 -> ContactEvolutions.addSignatures
  )

  override def createDefault(): Contact = {
    new Contact(IdHelper.generateContactId(), Seq(), IdHelper.generateMongoId(), Seq(), docVersion)
  }
}

object ContactEvolutions {

  val addContactType: Reads[JsObject] = Reads[JsObject] {
    js =>
      val addType = __.json.update((__ \ 'contactType).json.put(JsString(CONTACT_TYPE_INTERNAL)))
      val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(1)))
      js.transform(addType andThen addVersion)
  }

  val addSignatures: Reads[JsObject] = Reads {
    js =>
      {
        val addArray = __.json.update((__ \ 'signatures).json.put(JsArray()))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(2)))
        js.transform(addArray andThen addVersion)
      }
  }
}

case class ContactUpdate(groups: Option[Seq[String]],
                         signatures: Option[Seq[Signature]])

object ContactUpdate {
  implicit val format = Json.format[ContactUpdate]
}