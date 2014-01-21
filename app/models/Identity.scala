package models

import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.util.Date
import traits.{Model}
import play.api.libs.json.Reads._
import scala.concurrent.{ExecutionContext, Future}
import helper.IdHelper
import ExecutionContext.Implicits.global
import play.api.Logger

/**
 * User: Björn Reimer
 * Date: 1/16/14
 * Time: 4:19 PM
 */


case class Identity(
                     id: MongoId,
                     accountId: Option[MongoId],
                     displayName: Option[String],
                     userKey: String,
                     contacts: Seq[Contact],
                     conversations: Seq[MongoId],
                     assets: Seq[Asset],
                     tokens: Seq[MongoId],
                     created: Date,
                     lastUpdated: Date
                     ) {
  def toJson: JsValue = Json.toJson(this)(Identity.outputWrites)

  def addContact(contact: Contact) = {
    val query = Json.obj("_id" -> this.id)
    val set = Json.obj("$push" -> Json.obj("contacts" -> Json.obj("$each" -> Seq(contact), "$sort" -> Json.obj("name" -> 1), "$slice" -> (this.contacts.size + 5)*(-1))))
    Logger.debug("#######" + set)
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
        Json.obj("contacts" -> i.contacts) ++
        addCreated(i.created) ++
        addLastUpdated(i.lastUpdated)
  }

  def find(id: MongoId): Future[Option[Identity]] = {
    val query = Json.obj("_id" -> id)
    col.find(query).one[Identity]
  }

  def create(accountId: Option[MongoId]): MongoId = {
    val identity = new Identity(
      MongoId.create(),
      accountId,
      None,
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

  def getDisplayName(id: MongoId): Future[String] = {
    Identity.find(id).map {
      case None => "NoName"
      case Some(i) => i.displayName.getOrElse("NoName")
    }
  }
}


