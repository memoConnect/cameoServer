package models

import java.util.Date

import constants.Contacts._
import helper.JsonHelper._
import helper.MongoCollections._
import helper.{ IdHelper, JsonHelper }
import models.cockpit._
import models.cockpit.attributes._
import play.api.Play.current
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.{ Logger, Play }
import play.modules.reactivemongo.json.collection.JSONCollection
import services.AvatarGenerator
import traits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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
                    userKey: String,
                    contacts: Seq[Contact],
                    tokens: Seq[Token],
                    friendRequests: Seq[FriendRequest],
                    publicKeys: Seq[PublicKey],
                    ignoredIdentities: Seq[MongoId],
                    publicKeySignatures: Map[String, Signature], // Map: publicKeyId -> Signature
                    avatar: Option[MongoId],
                    isDefaultIdentity: Boolean = false,
                    created: Date,
                    lastUpdated: Date,
                    docVersion: Int) {

  def toPrivateJson: JsObject = Json.toJson(this)(Identity.privateWrites).as[JsObject]
  def toExternalJson: JsObject = Json.toJson(this)(Identity.externalWrites).as[JsObject]
  def toExternalOwnerJson: JsObject = Json.toJson(this)(Identity.externalOwnerWrites).as[JsObject]

  def toPublicJson(additionalPublicKeySignatures: Map[String, Signature] = Map()): JsObject = {
    Json.toJson(this)(Identity.publicWrites(additionalPublicKeySignatures)).as[JsObject]
  }

  private val query = Json.obj("_id" -> this.id)

  def addContact(contact: Contact): Future[Boolean] = {
    // check if this identity is already a contact
    this.contacts.find(_.identityId.equals(contact.identityId)) match {
      case Some(c) => Future(true)
      case None =>
        Contact.append(this.id, contact)
    }
  }

  def deleteContact(contactId: MongoId): Future[Boolean] = {
    Contact.delete(this.id, contactId).map(_.updatedExisting)
  }

  def getContactIdentities: Future[Seq[Identity]] = {
    val query = Json.obj("_id" -> Json.obj("$in" -> this.contacts.map(_.identityId)))
    Identity.findAll(query)
  }

  def addAsset(assetId: MongoId): Future[Boolean] = {
    val set = Json.obj("$addToSet" -> Json.obj("assets" -> assetId))
    Identity.col.update(query, set).map(_.updatedExisting)
  }

  def addToken(token: Token): Future[Boolean] = {
    Identity.addTokenToIdentity(this.id, token)
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
    update match {
      case PublicKeyUpdate(None) => Future(true)
      case PublicKeyUpdate(maybeName) =>
        val setValues = {
          maybeEmptyJson("publicKeys.$.name", maybeName) ++
            Json.obj("publicKeys.$.deleted" -> false)
        }
        val publicKeyQuery = query ++ Json.obj("publicKeys._id" -> id)
        val set = Json.obj("$set" -> setValues)
        Identity.col.update(publicKeyQuery, set).map {
          _.updatedExisting
        }
    }
  }

  def addPublicKeySignature(publicKeyId: String, signature: Signature): Future[Boolean] = {
    val set = Json.obj("$set" -> Json.obj(("publicKeySignatures." + publicKeyId) -> signature))
    Identity.col.update(query, set).map(_.updatedExisting)
  }

  def deletePublicKeySignature(publicKeyId: String): Future[Boolean] = {
    val set = Json.obj("$unset" -> Json.obj(("publicKeySignatures." + publicKeyId) -> ""))
    Identity.col.update(query, set).map(_.updatedExisting)
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

  def addSignatureToPublicKey(publicKeyId: MongoId, signature: Signature): Future[Boolean] = {
    def addSignature(): Future[Boolean] = {
      val query = Json.obj("_id" -> this.id, "publicKeys._id" -> publicKeyId)
      val set = Json.obj("$addToSet" -> Json.obj("publicKeys.$.signatures" -> signature))
      Identity.col.update(query, set).map(_.updatedExisting)
    }

    // check if this keyId has already signed this publicKey
    this.publicKeys.find(_.id.equals(publicKeyId)) match {
      case None => Future(false)
      case Some(key) =>
        key.signatures.find(_.keyId.equals(signature.keyId)) match {
          case None => addSignature()
          case Some(sig) =>
            // delete old signature
            this.deleteSignatureFromPublicKey(publicKeyId, signature.keyId).flatMap {
              res => addSignature()
            }
        }
    }
  }

  def deleteSignatureFromPublicKey(publicKeyId: MongoId, signatureKeyId: String): Future[Boolean] = {
    val query = Json.obj("_id" -> this.id, "publicKeys._id" -> publicKeyId)
    val set = Json.obj("$pull" -> Json.obj("publicKeys.$.signatures" -> Json.obj("keyId" -> signatureKeyId)))
    Identity.col.update(query, set).map(_.updatedExisting)
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
              val conversation = Conversation.create(subject = subject, recipients = Seq(supportIdentity.id, this.id).map(Recipient.create(_)))
              val message = Message.create(new MongoId(supportId), messageText)

              // create new conversation
              Conversation.col.insert(conversation).flatMap { le =>
                for {
                  // add contact and message
                  contactOk <- this.addContact(contact)
                  messageOk <- conversation.addMessage(message, supportIdentity.id)
                } yield {
                  contactOk && messageOk
                }
              }
          }

        case _ =>
          // config is not sufficient, do nothing
          Logger.error("Initial Support Contact not configured")
          Future(false)
      }
  }

  def getDisplayName: String = {
    this.displayName.getOrElse(this.cameoId)
  }

  def deleteDetails(deleteDisplayName: Boolean): Future[Boolean] = {
    val deleteValues: Seq[String] = Seq("email", "phoneNumber") ++ {
      if (deleteDisplayName) Seq("displayName") else Nil
    }

    Identity.deleteValues(this.id, deleteValues).map(_.updatedExisting)
  }
}

object Identity extends Model[Identity] with CockpitEditable[Identity] {

  implicit def col: JSONCollection = identityCollection

  implicit val mongoFormat: Format[Identity] = createMongoFormat(Json.reads[Identity], Json.writes[Identity])

  def createReads: Reads[Identity] = (
    Reads.pure[MongoId](IdHelper.generateIdentityId()) and
    Reads.pure[Option[MongoId]](None) and
    (__ \ 'displayName).readNullable[String] and
    (__ \ 'email).readNullable[VerifiedString](verifyMail andThen VerifiedString.createReads) and
    (__ \ 'phoneNumber).readNullable[VerifiedString](verifyPhoneNumber andThen VerifiedString.createReads) and
    ((__ \ 'cameoId).read[String] or Reads.pure[String](IdHelper.generateCameoId)) and
    Reads.pure[String](IdHelper.generateUserKey()) and
    Reads.pure[Seq[Contact]](Seq()) and
    Reads.pure[Seq[Token]](Seq()) and
    Reads.pure[Seq[FriendRequest]](Seq()) and
    Reads.pure[Seq[PublicKey]](Seq()) and
    Reads.pure[Seq[MongoId]](Seq()) and
    Reads.pure[Map[String, Signature]](Map()) and
    Reads.pure[Option[MongoId]](None) and
    Reads.pure[Boolean](false) and
    Reads.pure[Date](new Date()) and
    Reads.pure[Date](new Date()) and
    Reads.pure[Int](docVersion))(Identity.apply _)

  def privateWrites: Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.id.toJson) ++
        maybeEmptyJson("displayName", i.displayName) ++
        Json.obj("userKey" -> i.userKey) ++
        getCameoId(i.cameoId) ++
        maybeEmptyJson("email", i.email.map(_.toJson)) ++
        maybeEmptyJson("phoneNumber", i.phoneNumber.map(_.toJson)) ++
        Json.obj("publicKeys" -> i.publicKeys.filterNot(_.deleted).map(_.toJson())) ++
        Json.obj("userType" -> (if (i.accountId.isDefined) CONTACT_TYPE_INTERNAL else CONTACT_TYPE_EXTERNAL)) ++
        maybeEmptyJson("avatar", i.avatar.map(_.toJson)) ++
        addCreated(i.created) ++
        addLastUpdated(i.lastUpdated)
  }

  def publicWrites(additionalPublicKeySignatures: Map[String, Signature]): Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.id.toJson) ++
        getCameoId(i.cameoId) ++
        maybeEmptyJson("avatar", i.avatar.map(_.toJson)) ++
        maybeEmptyJson("displayName", i.displayName) ++
        Json.obj("publicKeys" -> i.publicKeys.filterNot(_.deleted).map(_.toJson(additionalPublicKeySignatures))) ++
        maybeEmptyJson("email", i.email.map(_.toJson)) ++
        maybeEmptyJson("phoneNumber", i.phoneNumber.map(_.toJson))
  }

  def externalWrites: Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.id.toJson) ++
        getCameoId(i.cameoId) ++
        maybeEmptyJson("avatar", i.avatar.map(_.toJson)) ++
        maybeEmptyJson("displayName", i.displayName) ++
        Json.obj("publicKeys" -> i.publicKeys.filterNot(_.deleted).map(_.toJson()))
  }

  def externalOwnerWrites: Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.id.toJson) ++
        getCameoId(i.cameoId) ++
        maybeEmptyJson("avatar", i.avatar.map(_.toJson)) ++
        maybeEmptyJson("displayName", i.displayName) ++
        maybeEmptyJson("email", i.email.map(_.toJson)) ++
        maybeEmptyJson("phoneNumber", i.phoneNumber.map(_.toJson)) ++
        Json.obj("publicKeys" -> i.publicKeys.filterNot(_.deleted).map(_.toJson()))
  }
  def getProjection(includeContacts: Boolean = false, includeTokens: Boolean = false): JsObject = {
    val contactProjection = if (includeContacts) Json.obj() else limitArray("contacts", 1, 0)
    val tokenProjection = if (includeTokens) Json.obj() else limitArray("tokens", 1, 0)
    contactProjection ++ tokenProjection
  }

  def create(accountId: Option[MongoId], cameoId: String, email: Option[String], phoneNumber: Option[String], isDefaultIdentity: Boolean = true, displayName: Option[String] = None): Identity = {
    new Identity(
      IdHelper.generateIdentityId(),
      accountId,
      displayName,
      VerifiedString.createOpt(email),
      VerifiedString.createOpt(phoneNumber),
      cameoId,
      IdHelper.generateUserKey(),
      Seq(),
      Seq(),
      Seq(),
      Seq(),
      Seq(),
      Map(),
      None,
      isDefaultIdentity,
      new Date,
      new Date,
      docVersion)
  }

  def createAndInsert(accountId: Option[MongoId], cameoId: String, email: Option[String], phoneNumber: Option[String], isDefaultIdentity: Boolean, displayName: Option[String] = None): Future[Identity] = {

    val identity = create(accountId, cameoId, email, phoneNumber, isDefaultIdentity: Boolean, displayName)

    // insert into db
    Identity.col.insert(identity).flatMap { le =>
      // generate default avatar
      AvatarGenerator.generate(identity).map {
        fileId => identity.copy(avatar = fileId)
      }
    }
  }

  override def find(id: MongoId): Future[Option[Identity]] = {
    val query = Json.obj("_id" -> id)
    val projection = getProjection()
    find(query, projection)
  }

  def findWith(id: MongoId, includeContacts: Boolean = false, includeTokens: Boolean = false): Future[Option[Identity]] = {
    val query = Json.obj("_id" -> id)
    val projection = getProjection(includeContacts, includeTokens)
    find(query, projection)
  }

  override def findAll(query: JsObject): Future[Seq[Identity]] = {
    val projection = getProjection()
    col.find(query, projection).cursor[Identity].collect[Seq]()
  }

  def findAllWith(query: JsObject, includeContacts: Boolean = false, includeTokens: Boolean = false): Future[Seq[Identity]] = {
    val projection = getProjection(includeContacts, includeTokens)
    col.find(query, projection).cursor[Identity].collect[Seq]()
  }

  def findByAccountId(id: MongoId): Future[Seq[Identity]] = {
    val query = Json.obj("accountId" -> id)
    findAll(query)
  }

  def findByToken(tokenId: MongoId, includeContacts: Boolean = false): Future[Option[Identity]] = {
    val query = Json.obj("tokens._id" -> tokenId)
    val projection = getProjection(includeContacts)
    find(query, projection)
  }

  def findByCameoId(cameoId: String): Future[Option[Identity]] = {
    // cameoIds are not case sensitive
    val query = Json.obj("cameoId" -> Json.obj("$regex" -> ("^" + cameoId + "$"), "$options" -> "i"))
    val projection = getProjection()

    find(query, projection)
  }

  def search(cameoId: Option[String], displayName: Option[String]): Future[Seq[Identity]] = {
    def toQueryOrEmpty(key: String, field: Option[String]): Seq[JsObject] = {
      field match {
        case None                     => Seq()
        case Some(f) if f.length == 3 => Seq(Json.obj(key -> f))
        case Some(f)                  => Seq(Json.obj(key -> Json.obj("$regex" -> f, "$options" -> "i")))
      }
    }

    val query = Json.obj(
      "$or" -> (toQueryOrEmpty("cameoId", cameoId) ++ toQueryOrEmpty("displayName", displayName)),
      "accountId" -> Json.obj("$exists" -> true)
    )

    val projection = Json.obj()

    col.find(query, projection).cursor[Identity].collect[Seq](upTo = 250)
  }

  def addTokenToIdentity(identityId: MongoId, token: Token): Future[Boolean] = {
    Token.append(identityId, token)
  }

  def createDefault(): Identity = {
    Identity.create(None, IdHelper.generateCameoId, None, None, false)
  }

  def evolutions = Map(
    0 -> IdentityEvolutions.addCameoId,
    1 -> IdentityEvolutions.addFriedRequest,
    2 -> IdentityEvolutions.addPublicKeys,
    3 -> IdentityEvolutions.removeConversations,
    4 -> IdentityEvolutions.removeAssets,
    5 -> IdentityEvolutions.convertFriendRequests,
    6 -> IdentityEvolutions.addIgnoredIdentities,
    7 -> IdentityEvolutions.addAuthenticationRequests,
    8 -> IdentityEvolutions.removeAuthenticationRequests,
    9 -> IdentityEvolutions.addDefaultIdentityFlag,
    10 -> IdentityEvolutions.addPublicKeySignatures
  )

  def cockpitMapping: Seq[CockpitAttribute] = {

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

object IdentityModelUpdate extends ModelUpdate {
  def values = Seq(
    StringUpdateValue("displayName", externalEdit = true),
    MongoIdUpdateValue("avatar", externalEdit = true),
    VerifiedStringUpdateValue("email", JsonHelper.verifyMail, externalEdit = true),
    VerifiedStringUpdateValue("phoneNumber", JsonHelper.verifyPhoneNumber, externalEdit = true),
    MongoIdUpdateValue("accountId"),
    BooleanUpdateValue("isDefaultIdentity"),
    StringUpdateValue("cameoId")
  )
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

  val addAuthenticationRequests: Reads[JsObject] = Reads {
    js =>
      {
        val addArray = __.json.update((__ \ 'authenticationRequests).json.put(JsArray()))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(8)))
        js.transform(addArray andThen addVersion)
      }
  }

  val removeAuthenticationRequests: Reads[JsObject] = Reads {
    js =>
      {
        val removeArray = (__ \ 'authenticationRequests).json.prune
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(9)))
        js.transform(removeArray andThen addVersion)
      }
  }

  val addDefaultIdentityFlag: Reads[JsObject] = Reads {
    js =>
      {
        val addBoolean = __.json.update((__ \ 'isDefaultIdentity).json.put(JsBoolean(true)))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(10)))
        js.transform(addBoolean andThen addVersion)
      }
  }

  val addPublicKeySignatures: Reads[JsObject] = Reads {
    js =>
      {
        val addObject = __.json.update((__ \ 'publicKeySignatures).json.put(Json.obj()))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(11)))
        js.transform(addObject andThen addVersion)
      }
  }
}