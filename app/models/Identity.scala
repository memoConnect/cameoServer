package models

import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.util.Date
import traits.{ CockpitEditable, Model }
import play.api.libs.json.Reads._
import scala.concurrent.{ ExecutionContext, Future }
import helper.{ PrintDate, IdHelper }
import ExecutionContext.Implicits.global
import constants.Messaging._
import constants.Contacts._
import reactivemongo.core.commands.LastError
import helper.JsonHelper._
import play.api.Logger
import helper.MongoCollections._
import reactivemongo.core.errors.DatabaseException
import models.cockpit.{CockpitList, CockpitListElement}

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
                    friendRequests: Seq[MongoId],
                    publicKeys: Seq[PublicKey],
                    created: Date,
                    lastUpdated: Date,
                    docVersion: Int) {

  def toPrivateJson: JsObject = Json.toJson(this)(Identity.privateWrites).as[JsObject]

  def toPublicSummaryJson: JsObject = Json.toJson(this)(Identity.publicSummaryWrites).as[JsObject]

  def toPublicJson: JsObject = Json.toJson(this)(Identity.publicWrites).as[JsObject]

  private val query = Json.obj("_id" -> this.id)

  def addContact(contact: Contact): Future[Boolean] = {
    try {
      val set = Json.obj("$push" -> Json.obj("contacts" -> Json.obj("$each" -> Seq(contact))))
      //    val set = Json.obj("$push" -> Json.obj("contacts" -> Json.obj("$each" -> Seq(contact), "$sort" -> Json.obj("name" -> 1), "$slice" -> (this.contacts.size + 5)*(-1))))
      Identity.col.update(query, set).map { _.updatedExisting }
    } catch {
      case e: DatabaseException => Future(false)
    }
  }

  def deleteContact(contactId: MongoId): Future[Boolean] = {
    val query = Json.obj("_id" -> this.id)
    val set = Json.obj("$pull" ->
      Json.obj("contacts" -> Json.obj("_id" -> contactId)))
    Identity.col.update(query, set).map { _.updatedExisting }
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

  def addFriendRequest(friendRequestId: MongoId): Future[LastError] = {
    val set = Json.obj("$addToSet" -> Json.obj("friendRequests" -> friendRequestId))
    Identity.col.update(query, set)
  }

  def deleteFriendRequest(friendRequestId: MongoId): Future[LastError] = {
    val set = Json.obj("$pull" -> Json.obj("friendRequests" -> friendRequestId))
    Identity.col.update(query, set)
  }

  def addPublicKey(publicKey: PublicKey): Future[Boolean] = {
    val set = Json.obj("$addToSet" -> Json.obj("publicKeys" -> publicKey))
    Identity.col.update(query, set).map { _.ok }
  }

  def deletePublicKey(id: MongoId): Future[Boolean] = {
    val set = Json.obj("$pull" -> Json.obj("publicKeys" -> Json.obj("_id" -> id)))
    Identity.col.update(query, set).map { _.updatedExisting }
  }

  def editPublicKey(id: MongoId, update: PublicKeyUpdate): Future[Boolean] = {
    val setValues = {
      toJsonOrEmpty("publicKeys.$.name", update.name) ++
        toJsonOrEmpty("publicKeys.$.key", update.key)
    }
    val publicKeyQuery = query ++ arrayQuery("publicKeys", id)
    val set = Json.obj("$set" -> setValues)
    Identity.col.update(publicKeyQuery, set).map { _.updatedExisting }
  }

  def update(update: IdentityUpdate): Future[Boolean] = {

    val newMail = update.email.flatMap { getNewValueVerifiedString(this.email, _) }
    val newPhoneNumber = update.phoneNumber.flatMap { getNewValueVerifiedString(this.phoneNumber, _) }
    val newDisplayName = update.displayName.flatMap { getNewValueString(this.displayName, _) }

    val setValues = {
      maybeEmpty("email", newMail.map { Json.toJson(_) }) ++
        maybeEmpty("phoneNumber", newPhoneNumber.map { Json.toJson(_) }) ++
        toJsonOrEmpty("displayName", newDisplayName)
    }
    val set = Json.obj("$set" -> setValues)

    Identity.col.update(query, set).map { _.updatedExisting }
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
    Reads.pure[Seq[MongoId]](Seq()) and
    Reads.pure[Seq[PublicKey]](Seq()) and
    Reads.pure[Date](new Date()) and
    Reads.pure[Date](new Date()) and
    Reads.pure[Int](docVersion))(Identity.apply _)

  def privateWrites: Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.id.toJson) ++
        toJsonOrEmpty("displayName", i.displayName) ++
        Json.obj("userKey" -> i.userKey) ++
        Json.obj("cameoId" -> i.cameoId) ++
        maybeEmpty("email", i.email.map { _.toJson }) ++
        maybeEmpty("phoneNumber", i.phoneNumber.map { _.toJson }) ++
        Json.obj("preferredMessageType" -> i.preferredMessageType) ++
        Json.obj("publicKeys" -> i.publicKeys.map { _.toJson }) ++
        Json.obj("userType" -> (if (i.accountId.isDefined) CONTACT_TYPE_INTERNAL else CONTACT_TYPE_EXTERNAL)) ++
        addCreated(i.created) ++
        addLastUpdated(i.lastUpdated)
  }

  def publicWrites: Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.id.toJson) ++
        Json.obj("cameoId" -> i.cameoId) ++
        maybeEmpty("email", i.email.map { _.toJson }) ++
        maybeEmpty("phoneNumber", i.phoneNumber.map { _.toJson }) ++
        Json.obj("displayName" -> JsString(i.displayName.getOrElse(IDENTITY_DEFAULT_DISPLAY_NAME))) ++
        Json.obj("publicKeys" -> i.publicKeys.map(pk => pk.key))
  }

  def publicSummaryWrites: Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.id.toJson) ++
        Json.obj("cameoId" -> i.cameoId) ++
        Json.obj("displayName" -> JsString(i.displayName.getOrElse(IDENTITY_DEFAULT_DISPLAY_NAME)))
  }

  def create(accountId: Option[MongoId], cameoId: String, email: Option[String], phoneNumber: Option[String]): Identity = {
    new Identity(
      IdHelper.generateIdentityId(),
      accountId,
      None,
      VerifiedString.createOpt(email),
      VerifiedString.createOpt(phoneNumber),
      cameoId,
      MESSAGE_TYPE_DEFAULT,
      IdHelper.generateUserKey(),
      Seq(),
      Seq(),
      Seq(),
      Seq(),
      new Date,
      new Date,
      docVersion)
  }

  def findToken(tokenId: MongoId): Future[Option[Identity]] = {
    val query = Json.obj("tokens" -> Json.obj("$elemMatch" -> Json.obj("_id" -> tokenId)))
    col.find(query).one[Identity]
  }

  def findCameoId(cameoId: String): Future[Option[Identity]] = {
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

  def docVersion = 5
  def evolutions = Map(
    0 -> IdentityEvolutions.addCameoId,
    1 -> IdentityEvolutions.addFriedRequest,
    2 -> IdentityEvolutions.addPublicKeys,
    3 -> IdentityEvolutions.removeConversations,
    4 -> IdentityEvolutions.removeAssets
  )

  def getList(limit: Int, offset: Int): Future[CockpitList] = {
    // todo use mongo aggregation framework
    col.find(Json.obj()).cursor[Identity].collect[Seq]().map { list =>
      val elements = list.map(toCockpitListElement)
      val titles = elements.headOption.map{_.getTitles}.getOrElse(Seq())
      new CockpitList(titles, elements)
    }
  }

  def toCockpitListElement(obj: Identity): CockpitListElement = {
    val id = obj.id.id
    val attributes = Map(
      "accountId" -> obj.accountId.map { _.toString },
      "displayName" -> obj.displayName,
      "email" -> obj.email.map { _.toJson.toString },
      "phoneNumber" -> obj.phoneNumber.map { _.toJson.toString },
      "cameoId" -> Some(obj.cameoId),
      "preferredMessageType" -> Some(obj.preferredMessageType),
      "userKey" -> Some(obj.userKey),
      "created" -> Some(PrintDate.toString(obj.created)),
      "lastUpdated" -> Some(PrintDate.toString(obj.lastUpdated)),
      "docVersion" -> Some(obj.docVersion.toString)
    )
    new CockpitListElement(id, attributes)
  }
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
}