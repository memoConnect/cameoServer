package models

import play.api.libs.json._
import play.api.libs.functional.syntax._
import helper.{ MongoCollections, IdHelper }
import traits.Model
import scala.concurrent.{ Future, ExecutionContext }
import ExecutionContext.Implicits.global
import play.api.mvc.{ Results, SimpleResult }
import play.mvc.Result
import helper.ResultHelper._
import play.api.Logger
import helper.JsonHelper._
import reactivemongo.core.commands.LastError
import constants.Contacts._

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 5:53 PM
 */
case class Contact(id: MongoId,
                   groups: Seq[String],
                   identityId: MongoId,
                   contactType: String,
                   docVersion: Int) {

  def toJson: JsObject = Json.toJson(this)(Contact.outputWrites).as[JsObject]

  def toJsonWithIdentity: Future[JsObject] =
    Identity.find(this.identityId).map {
      case None => Json.obj()
      case Some(identity) =>
        Json.toJson(this)(Contact.outputWrites).as[JsObject] ++
          Json.obj("identity" -> identity.toPublicJson)
    }

  def toJsonWithIdentityResult: Future[SimpleResult] = {
    this.toJsonWithIdentity.map(
      js => resOK(js))
  }

  def update(contactUpdate: ContactUpdate): Any = {

    // edit groups
    if (contactUpdate.groups.isDefined) {
      val query = arrayQuery("contacts", this.id)
      val newGroups = maybeEmptyJsValue("contacts.$.groups", contactUpdate.groups.map(Json.toJson(_)))
      val set = Json.obj("$set" -> newGroups)

      Contact.col.update(query, set).map {
        lastError => if (!lastError.updatedExisting) Logger.error("Could not update Contact: " + query + ":" + set)
      }
    }

    // edit identity only for external contacts
    if (this.contactType.equals(CONTACT_TYPE_EXTERNAL)) {
      val identityUpdate = new IdentityUpdate(VerifiedString.createOpt(contactUpdate.phoneNumber), VerifiedString.createOpt(contactUpdate.email), contactUpdate.displayName)

      Identity.find(this.identityId).map {
        case Some(identity) => identity.update(identityUpdate)
        case None           =>
      }
    }
  }

}

object Contact extends Model[Contact] {

  val col = MongoCollections.identityCollection

  implicit val mongoFormat: Format[Contact] = createMongoFormat(Json.reads[Contact], Json.writes[Contact])

  def createReads(identityId: MongoId, contactType: String): Reads[Contact] = (
    Reads.pure[MongoId](IdHelper.generateContactId()) and
    ((__ \ 'groups).read[Seq[String]] or Reads.pure(Seq[String]())) and
    Reads.pure[MongoId](identityId) and
    Reads.pure[String](contactType) and
    Reads.pure[Int](docVersion)
  )(Contact.apply _)

  def outputWrites: Writes[Contact] = Writes {
    c =>
      Json.obj("id" -> c.id.toJson) ++
        Json.obj("groups" -> c.groups) ++
        Json.obj("identityId" -> c.identityId.toJson) ++
        Json.obj("contactType" -> c.contactType)
  }

  def create(identityId: MongoId, contactType: String, groups: Seq[String] = Seq()): Contact = {
    new Contact(IdHelper.generateContactId(), groups, identityId, contactType, docVersion)
  }

  override def save(js: JsObject): Future[LastError] = {
    val id: MongoId = (js \ "_id").as[MongoId]
    val query = arrayQuery("contacts", id)
    val set = Json.obj("$set" -> Json.obj("contacts.$" -> js))
    col.update(query, set)
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
    new Contact(IdHelper.generateContactId(), Seq(), IdHelper.generateMongoId(), CONTACT_TYPE_NONE, docVersion)
  }
}

case class ContactUpdate(groups: Option[Seq[String]],
                         displayName: Option[String],
                         email: Option[String],
                         phoneNumber: Option[String])

object ContactUpdate {
  implicit val format = Json.format[ContactUpdate]
}