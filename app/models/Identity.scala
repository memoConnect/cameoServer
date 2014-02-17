package models

import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.util.Date
import traits.Model
import play.api.libs.json.Reads._
import scala.concurrent.{ ExecutionContext, Future }
import helper.IdHelper
import ExecutionContext.Implicits.global
import constants.Messaging._
import reactivemongo.core.commands.LastError
import helper.JsonHelper._
import play.api.Logger

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
                    conversations: Seq[MongoId],
                    assets: Seq[Asset],
                    tokens: Seq[MongoId],
                    created: Date,
                    lastUpdated: Date,
                    docVersion: Int) {

  def toJson: JsObject = Json.toJson(this)(Identity.outputWrites).as[JsObject]

  def toSummaryJson: JsObject = Json.toJson(this)(Identity.summaryWrites).as[JsObject]

  private val query = Json.obj("_id" -> this.id)

  def addContact(contact: Contact): Future[LastError] = {
    val set = Json.obj("$push" -> Json.obj("contacts" -> Json.obj("$each" -> Seq(contact))))
    //    val set = Json.obj("$push" -> Json.obj("contacts" -> Json.obj("$each" -> Seq(contact), "$sort" -> Json.obj("name" -> 1), "$slice" -> (this.contacts.size + 5)*(-1))))
    Identity.col.update(query, set)
  }

  def addConversation(conversationId: MongoId): Future[LastError] = {
    val set = Json.obj("$addToSet" -> Json.obj("conversations" -> conversationId))
    Identity.col.update(query, set)
  }

  def addAsset(assetId: MongoId): Future[LastError] = {
    val set = Json.obj("$addToSet" -> Json.obj("assets" -> assetId))
    Identity.col.update(query, set)
  }

  def addToken(tokenId: MongoId): Future[LastError] = {
    val set = Json.obj("$push" -> Json.obj("tokens" -> tokenId))
    Identity.col.update(query, set)
  }

  def update(update: IdentityUpdate): Future[LastError] = {

    val newMail = update.email.flatMap { getNewValueVerifiedString(this.email, _) }
    val newPhoneNumber = update.phoneNumber.flatMap { getNewValueVerifiedString(this.phoneNumber, _) }
    val newDisplayName = update.displayName.flatMap { getNewValueString(this.displayName, _) }

    val setValues = {
      maybeEmpty("email", newMail.map { Json.toJson(_) }) ++
        maybeEmpty("phoneNumber", newPhoneNumber.map { Json.toJson(_) }) ++
        toJsonOrEmpty("displayName", newDisplayName)
    }
    val set = Json.obj("$set" -> setValues)

    Identity.col.update(query, set)
  }

  def getGroup(groupName: String): Seq[Contact] = {
    this.contacts.filter(_.groups.contains(groupName))
  }

  def getGroups: Seq[String] = {
    this.contacts.flatMap(_.groups).distinct
  }
}

object Identity extends Model[Identity] {

  implicit def col = identityCollection
  val docVersion = 1

  val mongoReads = createMongoReads(Json.reads[Identity])
  val mongoWrites = createMongoWrites(Json.writes[Identity])

  implicit val mongoFormat: Format[Identity] = Format(mongoReads, mongoWrites)

  def createReads: Reads[Identity] = (
    Reads.pure[MongoId](IdHelper.generateIdentityId()) and
    Reads.pure[Option[MongoId]](None) and
    (__ \ 'displayName).readNullable[String] and
    (__ \ 'email).readNullable[VerifiedString](VerifiedString.createReads) and
    (__ \ 'phoneNumber).readNullable[VerifiedString](VerifiedString.createReads) and
    (__ \ 'cameoId).read[String] and
    ((__ \ 'preferredMessageType).read[String] or Reads.pure[String](MESSAGE_TYPE_DEFAULT)) and // TODO: check for right values
    Reads.pure[String](IdHelper.generateUserKey()) and
    Reads.pure[Seq[Contact]](Seq()) and
    Reads.pure[Seq[MongoId]](Seq()) and
    Reads.pure[Seq[Asset]](Seq()) and
    Reads.pure[Seq[MongoId]](Seq()) and
    Reads.pure[Date](new Date()) and
    Reads.pure[Date](new Date()) and
    Reads.pure[Int](docVersion))(Identity.apply _)

  def outputWrites: Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.id.toJson) ++
        toJsonOrEmpty("displayName", i.displayName) ++
        Json.obj("userKey" -> i.userKey) ++
        Json.obj("cameoId" -> i.cameoId) ++
        maybeEmpty("email", i.email.map {
          _.toJson
        }) ++
        maybeEmpty("phoneNumber", i.phoneNumber.map { _.toJson }) ++
        Json.obj("preferredMessageType" -> i.preferredMessageType) ++
        addCreated(i.created) ++
        addLastUpdated(i.lastUpdated)
  }

  def summaryWrites: Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.id.toJson) ++
        Json.obj("displayName" -> JsString(i.displayName.getOrElse(IDENTITY_DEFAULT_DISPLAY_NAME)))
  }

  override def find(id: MongoId): Future[Option[Identity]] = {
    val query = Json.obj("_id" -> id)
    col.find(query).one[JsObject].map {
      case None     => None
      case Some(js) => Some(readWithEvolutions(js))
    }
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

  def findCameoId(cameoId: String): Future[Option[Identity]] = {
    val query = Json.obj("cameoId" -> cameoId)
    col.find(query).one[Identity]
  }

  def matchCameoId(cameoId: String): Future[Seq[Identity]] = {
    val query = Json.obj("cameoId" -> Json.obj("$regex" -> cameoId))
    col.find(query).cursor[Identity].collect[Seq](1000, stopOnError = true)
  }

  // TODO: use the general approach: createMongoReadsWithEvolutions
  def readWithEvolutions(js: JsObject): Identity = {
    // catch exceptions and apply evolutions
    try {
      js.as[Identity]
    }
    catch {
      case JsResultException(e) =>
        // get document version, none == Version 0
        val currentDocVersion = (js \ "docVersion").asOpt[Int].getOrElse(0)
        val readsWithEvolution = getEvolutions(currentDocVersion)

        // update identity in db
        val newJs: JsObject = js.transform(readsWithEvolution).get
        Identity.col.save(newJs).map {
          lastError => if (!lastError.updatedExisting) {
            Logger.error("Error applying DB evolution to " + js)
          }
        }
        newJs.as[Identity]
    }
  }

  def getEvolutions(fromVersion: Int): Reads[JsObject] = {
    fromVersion match {
      case i if i == Identity.docVersion => __.json.pickBranch
      case i if i < Identity.docVersion => {
        evolutions(i) andThen getEvolutions(i + 1)
      }
    }
  }

  val evolutionAddCameoId: Reads[JsObject] = Reads {
    js =>
      {
        // getLoginName
        val addCameoId: Reads[JsObject] = __.json.update((__ \ 'cameoId).json.put(IdHelper.generateMessageId().toJson))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(1)))
        js.transform(addCameoId andThen addVersion)
      }
  }

  val evolutions: Map[Int, Reads[JsObject]] = Map(0 -> evolutionAddCameoId)

  // not used anymore
  val evolutionVerifiedMail: Reads[JsObject] = Reads {
    // convert mail and phoneNumber to verified string
    js =>
    {
      val convertMail: Reads[JsObject] = (js \ "email").asOpt[String] match {
        case None => __.json.pickBranch
        case Some(email) =>
          __.json.update((__ \ 'email).json.put(Json.toJson(VerifiedString.create(email))))
      }
      val convertPhoneNumber: Reads[JsObject] = (js \ "phoneNumber").asOpt[String] match {
        case None => __.json.pickBranch
        case Some(tel) =>
          __.json.update((__ \ 'phoneNumber).json.put(Json.toJson(VerifiedString.create(tel))))
      }
      val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(1)))
      js.transform(convertMail andThen convertPhoneNumber andThen addVersion)
    }
  }
}

case class IdentityUpdate(phoneNumber: Option[VerifiedString],
                          email: Option[VerifiedString],
                          displayName: Option[String])

object IdentityUpdate {
  implicit val format: Format[IdentityUpdate] = Json.format[IdentityUpdate]

  def create(phoneNumber: Option[VerifiedString] = None, email: Option[VerifiedString] = None, displayName: Option[String] = None): IdentityUpdate = {
    new IdentityUpdate(phoneNumber,email, displayName)
  }
}