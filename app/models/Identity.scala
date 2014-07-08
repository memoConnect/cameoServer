package models

import java.util.Date

import constants.Contacts._
import constants.Messaging._
import helper.IdHelper
import helper.JsonHelper._
import helper.MongoCollections._
import models.cockpit._
import models.cockpit.attributes.{ CockpitAttributeDate, CockpitAttributeString, CockpitAttributeVerifiedString, _ }
import play.api.Play.current
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{ JsArray, JsNumber, JsObject, _ }
import play.api.{ Logger, Play }
import services.AvatarGenerator
import traits.{ CockpitAttribute, CockpitEditable, Model }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ ExecutionContext, Future }

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
                    avatar: Option[MongoId],
                    created: Date,
                    lastUpdated: Date,
                    docVersion: Int) {

  def toPrivateJson: JsObject = Json.toJson(this)(Identity.privateWrites).as[JsObject]

  def toPublicJson: JsObject = Json.toJson(this)(Identity.publicWrites).as[JsObject]

  private val query = Json.obj("_id" -> this.id)

  def addContact(contact: Contact): Future[Boolean] = {
    // check if this identity is already a contact
    this.contacts.find(_.identityId.equals(contact.identityId)) match {
      case Some(c) => Future(true)
      case None =>
        Contact.append(this.id, contact).map(_.updatedExisting)
    }
  }

  def deleteContact(contactId: MongoId): Future[Boolean] = {
    Contact.delete(this.id, contactId).map(_.updatedExisting)
  }

  def addAsset(assetId: MongoId): Future[Boolean] = {
    val set = Json.obj("$addToSet" -> Json.obj("assets" -> assetId))
    Identity.col.update(query, set).map(_.updatedExisting)
  }

  def addToken(token: Token): Future[Boolean] = {
    Token.appendUnique(this.id, token).map(_.updatedExisting)
  }

  def deleteToken(tokenId: MongoId): Future[Boolean] = {
    Token.delete(this.id, tokenId).map(_.updatedExisting)
  }

  def addFriendRequest(friendRequest: FriendRequest): Future[Boolean] = {
    FriendRequest.appendUnique(this.id, friendRequest).map(_.updatedExisting)
  }

  def deleteFriendRequest(identityId: MongoId): Future[Boolean] = {
    FriendRequest.delete(this.id, identityId).map(_.updatedExisting)
  }

  def addPublicKey(publicKey: PublicKey): Future[Boolean] = {
    PublicKey.appendUnique(this.id, publicKey).map(_.updatedExisting)
  }

  def deletePublicKey(id: MongoId): Future[Boolean] = {
    PublicKey.delete(this.id, id).map(_.updatedExisting)
  }

  def editPublicKey(id: MongoId, update: PublicKeyUpdate): Future[Boolean] = {
    val setValues = {
      maybeEmptyString("publicKeys.$.name", update.name)
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
      getNewValue(this.displayName, _)
    }
    val newCameoId = update.cameoId
    val newAccountId = update.accountId.flatMap {
      getNewValue(this.accountId, _)
    }

    val setValues = {
      maybeEmptyJsValue("email", newMail.map(Json.toJson(_))) ++
        maybeEmptyJsValue("phoneNumber", newPhoneNumber.map(Json.toJson(_))) ++
        maybeEmptyString("displayName", newDisplayName) ++
        maybeEmptyString("cameoId", newCameoId) ++
        maybeEmptyJsValue("accountId", newAccountId.map(Json.toJson(_)))
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

  def setAvatar(id: MongoId): Future[Boolean] = {
    val set = Json.obj("$set" -> Json.obj("avatar" -> id))
    Identity.col.update(query, set).map(_.ok)
  }

  def addSupport(): Future[Boolean] = {
    (Play.configuration.getString("support.contact.identityId"),
      Play.configuration.getString("support.conversation.subject"),
      Play.configuration.getString("support.conversation.body")) match {
        case (Some(supportId), subject, Some(messageText)) =>
          Identity.find(supportId).flatMap {
            case None => Future(false)
            case Some(supportIdentity) =>
              val contact = Contact.create(supportIdentity.id, Seq())
              val conversation = Conversation.create(subject = subject, recipients = Seq(supportIdentity.id, this.id).map(Recipient.create))
              val message = Message.create(new MongoId(supportId), messageText)

              // create new conversation
              Conversation.col.insert(conversation).flatMap { le =>
                for {
                  // add contact and message
                  contactOk <- this.addContact(contact)
                  messageOk <- conversation.addMessage(message)
                } yield {
                  contactOk && messageOk
                }
              }
          }

        case _ =>
          // config is not sufficient, do nothing
          Logger.error("Inital Support Contact not configured")
          Future(false)
      }
  }

}

object Identity extends Model[Identity] with CockpitEditable[Identity] {

  implicit def col = identityCollection

  implicit val mongoFormat: Format[Identity] = createMongoFormat(Json.reads[Identity], Json.writes[Identity])

  // todo: this is now only used to create external contacts. rename and move accordingly
  def createReads: Reads[Identity] = (
    Reads.pure[MongoId](IdHelper.generateIdentityId()) and
    Reads.pure[Option[MongoId]](None) and
    (__ \ 'displayName).readNullable[String] and
    (__ \ 'email).readNullable[VerifiedString](verifyMail andThen VerifiedString.createReads) and
    (__ \ 'phoneNumber).readNullable[VerifiedString](verifyPhoneNumber andThen VerifiedString.createReads) and
    Reads.pure[String](IdHelper.generateCameoId) and
    ((__ \ 'preferredMessageType).read[String] or Reads.pure[String](MESSAGE_TYPE_DEFAULT)) and // TODO: check for right values
    Reads.pure[String](IdHelper.generateUserKey()) and
    Reads.pure[Seq[Contact]](Seq()) and
    Reads.pure[Seq[Token]](Seq()) and
    Reads.pure[Seq[FriendRequest]](Seq()) and
    Reads.pure[Seq[PublicKey]](Seq()) and
    Reads.pure[Seq[MongoId]](Seq()) and
    Reads.pure[Option[MongoId]](None) and
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
        maybeEmptyJsValue("avatar", i.avatar.map(_.toJson)) ++
        addCreated(i.created) ++
        addLastUpdated(i.lastUpdated)
  }

  def publicWrites: Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.id.toJson) ++
        Json.obj("cameoId" -> i.cameoId) ++
        maybeEmptyJsValue("avatar", i.avatar.map(_.toJson)) ++
        maybeEmptyString("displayName", i.displayName) ++
        Json.obj("publicKeys" -> i.publicKeys.map(_.toJson))
  }

  private def create(accountId: Option[MongoId], cameoId: String, email: Option[String], phoneNumber: Option[String], displayName: Option[String] = None): Identity = {
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
      None,
      new Date,
      new Date,
      docVersion)
  }

  def createAndInsert(accountId: Option[MongoId], cameoId: String, email: Option[String], phoneNumber: Option[String], displayName: Option[String] = None): Future[Identity] = {

    val identity = create(accountId, cameoId, email, phoneNumber, displayName)

    // insert into db
    Identity.col.insert(identity).map { le =>

      // generate default avatar
      AvatarGenerator.generate(identity)

      identity
    }
  }

  // todo: add projection to exclude contacts when not needed
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
        case Some(f) => Seq(Json.obj(key -> Json.obj("$regex" -> f, "$options" -> "i")))
      }
    }

    val query = Json.obj(
      "$or" -> (toQueryOrEmpty("cameoId", cameoId) ++ toQueryOrEmpty("displayName", displayName)),
      "accountId" -> Json.obj("$exists" -> true)
    )

    col.find(query).cursor[Identity].collect[Seq](upTo = 250)
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

case class IdentityUpdate(phoneNumber: Option[VerifiedString] = None,
                          email: Option[VerifiedString] = None,
                          displayName: Option[String] = None,
                          cameoId: Option[String] = None,
                          accountId: Option[MongoId] = None)

object IdentityUpdate {

  implicit val reads: Reads[IdentityUpdate] = (
    (__ \ "phoneNumber").readNullable[VerifiedString](verifyPhoneNumber andThen VerifiedString.createReads) and
    (__ \ "email").readNullable[VerifiedString](verifyMail andThen VerifiedString.createReads) and
    (__ \ "displayName").readNullable[String] and
    Reads.pure(None) and
    Reads.pure(None)
  )(IdentityUpdate.apply _)
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