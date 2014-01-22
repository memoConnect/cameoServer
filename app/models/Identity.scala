package models

import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.util.Date
import traits.Model
import play.api.libs.json.Reads._
import scala.concurrent.{ExecutionContext, Future}
import helper.IdHelper
import ExecutionContext.Implicits.global
import constants.Messaging._

/**
 * User: Björn Reimer
 * Date: 1/16/14
 * Time: 4:19 PM
 */


case class Identity(
                     id: MongoId,
                     accountId: Option[MongoId],
                     displayName: Option[String],
                     email: Option[String],
                     phoneNumber: Option[String],
                     preferredMessageType: String, // "mail" or "sms"
                     userKey: String,
                     contacts: Seq[Contact],
                     conversations: Seq[MongoId],
                     assets: Seq[Asset],
                     tokens: Seq[MongoId],
                     created: Date,
                     lastUpdated: Date
                     ) {

  def toJson: JsObject = Json.toJson(this)(Identity.outputWrites).as[JsObject]

  def toSummaryJson: JsObject = Json.toJson(this)(Identity.summaryWrites).as[JsObject]

  def addContact(contact: Contact) = {
    val query = Json.obj("_id" -> this.id)
    val set = Json.obj("$push" -> Json.obj("contacts" -> Json.obj("$each" -> Seq(contact))))
    //    val set = Json.obj("$push" -> Json.obj("contacts" -> Json.obj("$each" -> Seq(contact), "$sort" -> Json.obj("name" -> 1), "$slice" -> (this.contacts.size + 5)*(-1))))
    Identity.col.update(query, set)
  }

  def addConversation(conversationId: MongoId) = {
    val query = Json.obj("_id" -> this.id)
    val set = Json.obj("$addToSet" -> Json.obj("conversations" -> conversationId))
    Identity.col.update(query, set)
  }

  def addAsset(assetId: MongoId) = {
    val query = Json.obj("_id" -> this.id)
    val set = Json.obj("$addToSet" -> Json.obj("assets" -> assetId))
    Identity.col.update(query, set)
  }

  def addToken(tokenId: MongoId) = {
    val query = Json.obj("_id" -> this.id)
    val set = Json.obj("$push" -> Json.obj("tokens" -> tokenId))
    Identity.col.update(query, set)
  }

}


object Identity extends Model[Identity] {

  implicit def col = identityCollection

  implicit val mongoFormat: Format[Identity] = createMongoFormat(Json.reads[Identity], Json.writes[Identity])

  def createReads: Reads[Identity] = (
    Reads.pure[MongoId](IdHelper.generateIdentityId()) and
      Reads.pure[Option[MongoId]](None) and
      (__ \ 'displayName).readNullable[String] and
      (__ \ 'email).readNullable[String] and
      (__ \ 'phoneNumber).readNullable[String] and
      ((__ \ 'preferredMessageType).read[String] or Reads.pure[String](MESSAGE_TYPE_DEFAULT)) and // TODO: check for right values
      Reads.pure[String](IdHelper.generateUserKey()) and
      Reads.pure[Seq[Contact]](Seq()) and
      Reads.pure[Seq[MongoId]](Seq()) and
      Reads.pure[Seq[Asset]](Seq()) and
      Reads.pure[Seq[MongoId]](Seq()) and
      Reads.pure[Date](new Date()) and
      Reads.pure[Date](new Date())
    )(Identity.apply _)

  def outputWrites: Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.id.toJson) ++
        toJsonOrEmpty("displayName", i.displayName) ++
        Json.obj("userKey" -> i.userKey) ++
        Json.obj("contacts" -> i.contacts.map(_.toJson)) ++
        toJsonOrEmpty("email", i.email) ++
        toJsonOrEmpty("phoneNumber", i.phoneNumber) ++
        Json.obj("preferredMessageType" -> i.preferredMessageType) ++
        addCreated(i.created) ++
        addLastUpdated(i.lastUpdated)
  }

  def summaryWrites: Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.id.toJson) ++
        Json.obj("displayName" -> JsString(i.displayName.getOrElse(IDENTITY_DEFAULT_DISPLAY_NAME)))
  }

  def find(id: MongoId): Future[Option[Identity]] = {
    val query = Json.obj("_id" -> id)
    col.find(query).one[Identity]
  }

  def create(accountId: Option[MongoId], email: Option[String], phoneNumber: Option[String]): MongoId = {
    val identity = new Identity(
      IdHelper.generateIdentityId(),
      accountId,
      None,
      email,
      phoneNumber,
      MESSAGE_TYPE_DEFAULT,
      IdHelper.generateUserKey(),
      Seq(),
      Seq(),
      Seq(),
      Seq(),
      new Date,
      new Date
    )
    col.insert(identity)
    identity.id
  }
}


