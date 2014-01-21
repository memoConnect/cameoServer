package models

import java.util.Date
import play.api.libs.json._
import play.api.libs.functional.syntax._
import helper.IdHelper
import traits.{Model}

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 5:53 PM
 */
case class Contact(
                    id: MongoId,
                    name: String,
                    email: Option[String],
                    phonenumber: Option[String],
                    groups: Seq[String],
                    identityId: Option[MongoId],
                    created: Date,
                    lastUpdated: Date
                    ) {
  def toJson: JsValue = Json.toJson(this)(Contact.outputWrites)

}

object Contact extends Model[Contact] {

  implicit val col = userCollection
  implicit val mongoFormat: Format[Contact] = createMongoFormat(Json.reads[Contact], Json.writes[Contact])

  def createReads: Reads[Contact] = (
    Reads.pure[MongoId](new MongoId(IdHelper.generateContactId())) and
      ((__ \ 'name).read[String] or Reads.pure(IdHelper.generateContactId())) and
      (__ \ 'email).readNullable[String] and
      (__ \ 'phonenumber).readNullable[String] and
      ((__ \ 'groups).read[Seq[String]] or Reads.pure(Seq[String]())) and
      // TODO: check if identity exists
      (__ \ 'identity).readNullable[MongoId] and
      Reads.pure[Date](new Date) and
      Reads.pure[Date](new Date)
    )(Contact.apply _)

  def outputWrites: Writes[Contact] = Writes {
    c =>
      Json.obj("id" -> c.id.toJson) ++
      Json.obj("name" -> c.name) ++
        toJsonOrEmpty("email", c.email) ++
        toJsonOrEmpty("phonenumber", c.phonenumber) ++
        Json.obj("groups" -> c.groups) ++
        addCreated(c.created) ++
        addLastUpdated(c.lastUpdated)
  }

  override val sortWith = {
    (c1: Contact, c2: Contact) => c1.name < c2.name
  }
}

