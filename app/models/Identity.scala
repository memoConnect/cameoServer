package models

import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.util.Date
import traits.{ CockpitAttribute, CockpitEditable, Model }
import play.api.libs.json.Reads._
import scala.concurrent.{ ExecutionContext, Future }
import ExecutionContext.Implicits.global
import constants.Contacts._
import reactivemongo.core.commands._
import helper.JsonHelper._
import models.cockpit._
import play.api.libs.json.JsArray
import scala.Some
import play.api.libs.json.JsNumber
import play.api.libs.json.JsObject
import models.cockpit.attributes._
import play.api.libs.json.JsArray
import play.api.libs.json.JsString
import scala.Some
import play.api.libs.json.JsNumber
import play.api.libs.json.JsObject
import helper.MongoCollections._
import helper.IdHelper
import constants.Messaging._
import models.cockpit.attributes.CockpitAttributeDate
import play.api.libs.json.JsArray
import scala.Some
import play.api.libs.json.JsNumber
import models.cockpit.attributes.CockpitAttributeString
import models.cockpit.attributes.CockpitAttributeVerifiedString
import play.api.libs.json.JsObject

/**
 * User: Björn Reimer
 * Date: 1/16/14
 * Time: 4:19 PM
 */

case class Identity(id: MongoId,
                    accountId: Option[MongoId],
                    displayName: Option[String],
                    email: Option[VerifiedString],
                    phoneNumber: Option[VerifiedString],
                    cameoId: String,
                    preferredMessageType: String, // "mail" or "sms"
                    userKey: String,
                    contacts: Seq[Contact],
                    tokens: Seq[Token],
                    friendRequests: Seq[FriendRequest],
                    publicKeys: Seq[PublicKey],
                    ignoredIdentities: Seq[MongoId],
                    created: Date,
                    lastUpdated: Date,
                    docVersion: Int) {

  def toPrivateJson: JsObject = Json.toJson(this)(Identity.privateWrites).as[JsObject]

  def toPublicSummaryJson: JsObject = Json.toJson(this)(Identity.publicSummaryWrites).as[JsObject]

  def toPublicJson: JsObject = Json.toJson(this)(Identity.publicWrites).as[JsObject]

  private val query = Json.obj("_id" -> this.id)

  def addContact(contact: Contact): Future[Boolean] = {

    // check if this identity is already a contact
    this.contacts.find(_.identityId.equals(contact.identityId)) match {
      case Some(c) => Future(true)
      case None =>
        val set = Json.obj("$push" -> Json.obj("contacts" -> Json.obj("$each" -> Seq(contact))))
        Identity.col.update(query, set).map {
          _.updatedExisting
        }
    }
  }

  def deleteContact(contactId: MongoId): Future[Boolean] = {
    val query = Json.obj("_id" -> this.id)
    val set = Json.obj("$pull" ->
      Json.obj("contacts" -> Json.obj("_id" -> contactId)))
    Identity.col.update(query, set).map {
      _.updatedExisting
    }
  }

  def addAsset(assetId: MongoId): Future[LastError] = {
    val set = Json.obj("$addToSet" -> Json.obj("assets" -> assetId))
    Identity.col.update(query, set)
  }

  def addToken(token: Token): Future[LastError] = {
    val set = Json.obj("$addToSet" -> Json.obj("tokens" -> token))
    Identity.col.update(query, set)
  }

  def deleteToken(tokenId: MongoId): Future[LastError] = {
    val set = Json.obj("$pull" -> Json.obj("$elemMatch" -> Json.obj("tokens" -> tokenId)))
    Identity.col.update(query, set)
  }

  def addFriendRequest(friendRequest: FriendRequest): Future[LastError] = {
    val set = Json.obj("$addToSet" -> Json.obj("friendRequests" -> friendRequest))
    Identity.col.update(query, set)
  }

  def deleteFriendRequest(identityId: MongoId): Future[LastError] = {
    val set = Json.obj("$pull" -> Json.obj("friendRequests" -> Json.obj("identityId" -> identityId)))
    Identity.col.update(query, set)
  }

  def addPublicKey(publicKey: PublicKey): Future[Boolean] = {
    val set = Json.obj("$addToSet" -> Json.obj("publicKeys" -> publicKey))
    Identity.col.update(query, set).map {
      _.ok
    }
  }

  def deletePublicKey(id: MongoId): Future[Boolean] = {
    val set = Json.obj("$pull" -> Json.obj("publicKeys" -> Json.obj("_id" -> id)))
    Identity.col.update(query, set).map {
      _.updatedExisting
    }
  }

  def editPublicKey(id: MongoId, update: PublicKeyUpdate): Future[Boolean] = {
    val setValues = {
      maybeEmptyString("publicKeys.$.name", update.name) ++
        maybeEmptyString("publicKeys.$.key", update.key) ++
        maybeEmptyInt("publicKeys.$.keySize", update.keySize)
    }
    val publicKeyQuery = query ++ arrayQuery("publicKeys", id)
    val set = Json.obj("$set" -> setValues)
    Identity.col.update(publicKeyQuery, set).map {
      _.updatedExisting
    }
  }

  def addIgnored(identityId: MongoId): Future[Boolean] = {
    val set = Json.obj("$addToSet" -> Json.obj("ignoredIdentities" -> identityId))
    Identity.col.update(query, set).map {
      _.ok
    }
  }

  def deleteIgnored(identityId: MongoId): Future[Boolean] = {
    val set = Json.obj("$pull" -> Json.obj("ignoredIdentities" -> identityId))
    Identity.col.update(query, set).map {
      _.updatedExisting
    }
  }


  def update(update: IdentityUpdate): Future[Boolean] = {

    val newMail = update.email.flatMap {
      getNewValueVerifiedString(this.email, _)
    }
    val newPhoneNumber = update.phoneNumber.flatMap {
      getNewValueVerifiedString(this.phoneNumber, _)
    }
    val newDisplayName = update.displayName.flatMap {
      getNewValueString(this.displayName, _)
    }

    val setValues = {
      maybeEmptyJsValue("email", newMail.map {
        Json.toJson(_)
      }) ++
        maybeEmptyJsValue("phoneNumber", newPhoneNumber.map {
          Json.toJson(_)
        }) ++
        maybeEmptyString("displayName", newDisplayName)
    }
    val set = Json.obj("$set" -> setValues)

    Identity.col.update(query, set).map {
      _.updatedExisting
    }
  }

  def getGroup(groupName: String): Seq[Contact] = {
    this.contacts.filter(_.groups.contains(groupName))
  }

  def getGroups: Seq[String] = {
    this.contacts.flatMap(_.groups).distinct
  }
}

object Identity extends Model[Identity] with CockpitEditable[Identity] {

  implicit def col = identityCollection

  implicit val mongoFormat: Format[Identity] = createMongoFormat(Json.reads[Identity], Json.writes[Identity])

  def createReads: Reads[Identity] = (
    Reads.pure[MongoId](IdHelper.generateIdentityId()) and
    Reads.pure[Option[MongoId]](None) and
    (__ \ 'displayName).readNullable[String] and
    (__ \ 'email).readNullable[VerifiedString](verifyMail andThen VerifiedString.createReads) and
    (__ \ 'phoneNumber).readNullable[VerifiedString](verifyPhoneNumber andThen VerifiedString.createReads) and
    ((__ \ 'cameoId).read[String] or Reads.pure[String](IdHelper.generateCameoId)) and
    ((__ \ 'preferredMessageType).read[String] or Reads.pure[String](MESSAGE_TYPE_DEFAULT)) and // TODO: check for right values
    Reads.pure[String](IdHelper.generateUserKey()) and
    Reads.pure[Seq[Contact]](Seq()) and
    Reads.pure[Seq[Token]](Seq()) and
    Reads.pure[Seq[FriendRequest]](Seq()) and
    Reads.pure[Seq[PublicKey]](Seq()) and
    Reads.pure[Seq[MongoId]](Seq()) and
    Reads.pure[Date](new Date()) and
    Reads.pure[Date](new Date()) and
    Reads.pure[Int](docVersion))(Identity.apply _)

  def privateWrites: Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.id.toJson) ++
        maybeEmptyString("displayName", i.displayName) ++
        Json.obj("userKey" -> i.userKey) ++
        Json.obj("cameoId" -> i.cameoId) ++
        maybeEmptyJsValue("email", i.email.map {
          _.toJson
        }) ++
        maybeEmptyJsValue("phoneNumber", i.phoneNumber.map {
          _.toJson
        }) ++
        Json.obj("preferredMessageType" -> i.preferredMessageType) ++
        Json.obj("publicKeys" -> i.publicKeys.map {
          _.toJson
        }) ++
        Json.obj("userType" -> (if (i.accountId.isDefined) CONTACT_TYPE_INTERNAL else CONTACT_TYPE_EXTERNAL)) ++
        addCreated(i.created) ++
        addLastUpdated(i.lastUpdated)
  }

  def publicWrites: Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.id.toJson) ++
        Json.obj("cameoId" -> i.cameoId) ++
        maybeEmptyJsValue("email", i.email.map {
          _.toJson
        }) ++
        maybeEmptyJsValue("phoneNumber", i.phoneNumber.map {
          _.toJson
        }) ++
        maybeEmptyString("displayName", i.displayName) ++
        Json.obj("publicKeys" -> i.publicKeys.map(_.toJson))
  }

  def publicSummaryWrites: Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.id.toJson) ++
        Json.obj("cameoId" -> i.cameoId) ++
        maybeEmptyString("displayName", i.displayName)
  }

  def create(accountId: Option[MongoId], cameoId: String, email: Option[String], phoneNumber: Option[String], displayName: Option[String] = None): Identity = {
    new Identity(
      IdHelper.generateIdentityId(),
      accountId,
      displayName,
      VerifiedString.createOpt(email),
      VerifiedString.createOpt(phoneNumber),
      cameoId,
      MESSAGE_TYPE_DEFAULT,
      IdHelper.generateUserKey(),
      Seq(),
      Seq(),
      Seq(),
      Seq(),
      Seq(),
      new Date,
      new Date,
      docVersion)
  }

  def findByToken(tokenId: MongoId): Future[Option[Identity]] = {
    val query = Json.obj("tokens" -> Json.obj("$elemMatch" -> Json.obj("_id" -> tokenId)))
    col.find(query).one[Identity]
  }

  def findByCameoId(cameoId: String): Future[Option[Identity]] = {
    val query = Json.obj("cameoId" -> cameoId)
    col.find(query).one[Identity]
  }

  def search(cameoId: Option[String], displayName: Option[String]): Future[Seq[Identity]] = {

    def toQueryOrEmpty(key: String, field: Option[String]): Seq[JsObject] = {
      field match {
        case None    => Seq()
        case Some(f) => Seq(Json.obj(key -> Json.obj("$regex" -> f)))
      }
    }

    val query = Json.obj("$or" -> (toQueryOrEmpty("cameoId", cameoId) ++ toQueryOrEmpty("displayName", displayName)))

    col.find(query).cursor[Identity].collect[Seq]()
  }

  def createDefault(): Identity = {
    Identity.create(None, IdHelper.generateCameoId, None, None)
  }

  def docVersion = 7

  def evolutions = Map(
    0 -> IdentityEvolutions.addCameoId,
    1 -> IdentityEvolutions.addFriedRequest,
    2 -> IdentityEvolutions.addPublicKeys,
    3 -> IdentityEvolutions.removeConversations,
    4 -> IdentityEvolutions.removeAssets,
    5 -> IdentityEvolutions.convertFriendRequests,
    6 -> IdentityEvolutions.addIgnoredIdentities
  )

  def cockpitMapping: Seq[CockpitAttribute] = {
    val pmtOptions = Seq(MESSAGE_TYPE_DEFAULT, MESSAGE_TYPE_EMAIL, MESSAGE_TYPE_SMS)

    Seq(
      CockpitAttributeFilter(name = "accountId", displayName = "Account Id", listName = "account", filterName = "ID"),
      CockpitAttributeString[Option[String]](name = "displayName", displayName = "Display Name", isEditable = true, showInList = true, nullValue = None),
      CockpitAttributeString[String](name = "cameoId", displayName = "Cameo Id", nullValue = "", showInList = true),
      CockpitAttributeVerifiedString(name = "phoneNumber", displayName = "Phone Number", isEditable = true, showInList = true),
      CockpitAttributeVerifiedString(name = "email", displayName = "Email", isEditable = true, showInList = true),
      CockpitAttributeString[String](name = "preferredMessageType", displayName = "Preferred Message Type", nullValue = "", isEditable = true),
      CockpitAttributeString[String](name = "userKey", displayName = "User Key", nullValue = ""),
      CockpitAttributeFilter("contacts", "Contacts", "identity", "ID"),
      CockpitAttributeSimpleList("tokens", "Tokens"),
      CockpitAttributeSimpleList("friendRequests", "Friend Requests"),
      CockpitAttributeSimpleList("publicKeys", "Public Keys"),
      CockpitAttributeDate(name = "created", displayName = "Created"),
      CockpitAttributeDate(name = "lastUpdated", displayName = "Last Updated (not working yet)")
    )
  }

  def cockpitListFilters: Seq[CockpitListFilter] = Seq(
    new CockpitListFilter("ID", str => Json.obj("_id.mongoId" -> Json.obj("$regex" -> str))),
    new CockpitListFilter("Email", str => Json.obj("email.value" -> Json.obj("$regex" -> str))),
    new CockpitListFilter("PhoneNumber", str => Json.obj("phoneNumber.value" -> Json.obj("$regex" -> str))),
    new CockpitListFilter("DisplayName", str => Json.obj("displayName" -> Json.obj("$regex" -> str))),
    new CockpitListFilter("CameoId", str => Json.obj("cameoId" -> Json.obj("$regex" -> str)))
  )

}

case class IdentityUpdate(phoneNumber: Option[VerifiedString],
                          email: Option[VerifiedString],
                          displayName: Option[String])

object IdentityUpdate {

  implicit val reads: Reads[IdentityUpdate] = (
    (__ \ "phoneNumber").readNullable[VerifiedString](verifyPhoneNumber andThen VerifiedString.createReads) and
    (__ \ "email").readNullable[VerifiedString](verifyMail andThen VerifiedString.createReads) and
    (__ \ "displayName").readNullable[String]
  )(IdentityUpdate.apply _)

  def create(phoneNumber: Option[VerifiedString] = None, email: Option[VerifiedString] = None, displayName: Option[String] = None): IdentityUpdate = {
    new IdentityUpdate(phoneNumber, email, displayName)
  }
}

object IdentityEvolutions {

  val addCameoId: Reads[JsObject] = Reads {
    js =>
      {
        val addCameoId: Reads[JsObject] = __.json.update((__ \ 'cameoId).json.put(IdHelper.generateMessageId().toJson))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(1)))
        js.transform(addCameoId andThen addVersion)
      }
  }

  val addFriedRequest: Reads[JsObject] = Reads {
    js =>
      {
        val addFriendRequest: Reads[JsObject] = __.json.update((__ \ 'friendRequests).json.put(JsArray()))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(2)))
        js.transform(addFriendRequest andThen addVersion)
      }
  }

  val addPublicKeys: Reads[JsObject] = Reads {
    js =>
      {
        val addFriendRequest: Reads[JsObject] = __.json.update((__ \ 'publicKeys).json.put(JsArray()))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(3)))
        js.transform(addFriendRequest andThen addVersion)
      }
  }

  val removeConversations: Reads[JsObject] = Reads {
    js =>
      {
        val removeConversations: Reads[JsObject] = (__ \ 'conversations).json.prune
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(4)))
        js.transform(removeConversations andThen addVersion)
      }
  }

  val removeAssets: Reads[JsObject] = Reads {
    js =>
      {
        val removeAssets: Reads[JsObject] = (__ \ 'assets).json.prune
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(5)))
        js.transform(removeAssets andThen addVersion)
      }
  }

  val convertFriendRequests: Reads[JsObject] = Reads {
    js =>
    {
      val resetFriendRequests = __.json.update((__ \ 'friendRequests).json.put(JsArray()))
      val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(6)))
      js.transform(resetFriendRequests andThen addVersion)
    }
  }

  val addIgnoredIdentities: Reads[JsObject] = Reads {
    js =>
    {
      val addArray = __.json.update((__ \ 'ignoredIdentities).json.put(JsArray()))
      val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(7)))
      js.transform(addArray andThen addVersion)
    }
  }
}