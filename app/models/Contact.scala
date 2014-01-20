package models

import java.util.Date
import play.api.libs.json._
import play.api.libs.functional.syntax._
import helper.IdHelper
import traits.{OutputLimits, MongoHelper, Model}

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 5:53 PM
 */
case class Contact (
                    contactId: String,
                    name: String,
                    email: Option[String],
                    phonenumber: Option[String],
                    groups: Seq[String],
                    username: Option[String],
                    created: Date,
                    lastUpdated: Date
                  )      {
  def toJson:JsValue = Json.toJson(this)(Contact.outputWrites)

}

object Contact extends Model[Contact]
{

  implicit val col = userCollection
  implicit val mongoFormat: Format[Contact] = createMongoFormat(Json.reads[Contact], Json.writes[Contact])

  // Input/output format for the API
  def inputReads: Reads[Contact] = (
    Reads.pure[String](IdHelper.generateContactId()) and
    ((__ \ 'name).read[String] or Reads.pure(IdHelper.generateContactId())) and
    (__ \ 'email).readNullable[String] and
    (__ \ 'phonenumber).readNullable[String] and
    ((__ \ 'groups).read[Seq[String]] or Reads.pure(Seq[String]())) and
    (__ \ 'username).readNullable[String] and
    Reads.pure[Date](new Date) and
    Reads.pure[Date](new Date)
    )(Contact.apply _)

  def outputWrites(implicit ol: OutputLimits = OutputLimits(0,0)): Writes[Contact] = Writes {
    contact =>
      Json.obj("name" -> contact.name) ++
        toJsonOrEmpty("email", contact.email) ++
        toJsonOrEmpty("phonenumber", contact.phonenumber) ++
        toJsonOrEmpty("username", contact.username) ++
        Json.obj("groups" -> contact.groups) ++
        Json.obj("contactId" -> contact.contactId) ++
        addLastUpdated(contact.lastUpdated)
  }

  override val sortWith = {
    (c1: Contact, c2: Contact) => c1.name < c2.name
  }
}

