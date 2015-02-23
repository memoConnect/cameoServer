package models

import java.util.Date

import helper.JsonHelper._
import helper.MongoCollections._
import helper.{ IdHelper, JsonHelper }
import models.cockpit.CockpitListFilter
import models.cockpit.attributes._
import play.api.Play
import play.api.Play.current
import play.api.libs.json.Reads._
import play.api.libs.json._
import reactivemongo.core.commands.LastError
import traits._
import play.api.libs.functional.syntax._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 1/16/14
 * Time: 4:19 PM
 */
case class Account(id: MongoId,
                   loginName: String,
                   password: String,
                   phoneNumber: Option[VerifiedString],
                   email: Option[VerifiedString],
                   properties: AccountProperties,
                   userSettings: AccountUserSettings,
                   registrationIncomplete: Option[Boolean],
                   created: Date,
                   lastUpdated: Date,
                   docVersion: Int) {

  def toJson: JsObject = Json.toJson(this)(Account.outputWrites).as[JsObject]

  def toJsonWithIdentities(activeIdentityId: MongoId): Future[JsObject] = {
    Identity.findAll(Json.obj("accountId" -> this.id)).map {
      list =>
        this.toJson ++ Json.obj("identities" -> list.map {
          identity =>
            val isActive: Boolean = identity.id.equals(activeIdentityId)
            identity.toPrivateJson ++ Json.obj("active" -> isActive)
        })
    }
  }
}

object Account extends Model[Account] with CockpitEditable[Account] {

  def col = accountCollection

  implicit val mongoFormat: Format[Account] = createMongoFormat(Json.reads[Account], Json.writes[Account])

  def outputWrites: Writes[Account] = Writes {
    a =>
      Json.obj("id" -> a.id.toJson) ++
        Json.obj("loginName" -> a.loginName) ++
        maybeEmptyJson("phoneNumber", a.phoneNumber.map(_.toJson)) ++
        maybeEmptyJson("email", a.email.map(_.toJson)) ++
        Json.obj("userSettings" -> a.userSettings) ++
        maybeEmptyJson("registrationIncomplete", a.registrationIncomplete) ++
        addCreated(a.created) ++
        addLastUpdated(a.lastUpdated)
  }

  // deprecated. ToDo: delete when legacy code is deleted
  def createReads(): Reads[Account] = {
    val id = IdHelper.generateAccountId()
    (Reads.pure[MongoId](id) and
      (__ \ 'loginName).read[String] and
      (__ \ 'password).read[String](minLength[String](8) andKeep hashPassword) and
      (__ \ 'phoneNumber).readNullable[VerifiedString](verifyPhoneNumber andThen VerifiedString.createReads) and
      (__ \ 'email).readNullable[VerifiedString](verifyMail andThen VerifiedString.createReads) and
      Reads.pure[AccountProperties](AccountProperties.defaultProperties) and
      Reads.pure[AccountUserSettings](AccountUserSettings.defaultSettings) and
      Reads.pure[Option[Boolean]](Some(true)) and
      Reads.pure[Date](new Date()) and
      Reads.pure[Date](new Date()) and
      Reads.pure[Int](docVersion)
    )(Account.apply _)
  }

  def findByLoginName(loginName: String): Future[Option[Account]] = {
    val query = Json.obj("loginName" -> loginName.toLowerCase)
    find(query)
  }

  def create(loginName: String, password: String, phoneNumber: Option[VerifiedString] = None, email: Option[VerifiedString] = None): Account = {
    new Account(
      IdHelper.generateAccountId(),
      loginName.toLowerCase,
      password,
      phoneNumber,
      email,
      AccountProperties.defaultProperties,
      AccountUserSettings.defaultSettings,
      Some(true),
      new Date,
      new Date,
      docVersion
    )
  }

  def createDefault(): Account = {
    this.create(IdHelper.randomString(8), "")
  }

  def cockpitMapping: Seq[CockpitAttribute] = {
    Seq(
      CockpitAttributeString[String](name = "loginName", displayName = "Login Name", nullValue = "", showInList = true),
      CockpitAttributeString[String](name = "password", displayName = "Password", nullValue = ""),
      CockpitAttributeString[Option[String]](name = "phoneNumber", displayName = "Phone Number", nullValue = None, isEditable = true, showInList = true),
      CockpitAttributeString[Option[String]](name = "email", displayName = "Email", nullValue = None, isEditable = true, showInList = true),
      CockpitAttributeFilter("identities", "Identities", "identity", "ID"),
      CockpitAttributeDate(name = "created", displayName = "Created"),
      CockpitAttributeDate(name = "lastUpdated", displayName = "Last Updated (not working yet)")
    )
  }

  def cockpitListFilters: Seq[CockpitListFilter] = Seq(
    new CockpitListFilter("ID", str => Json.obj("_id.mongoId" -> Json.obj("$regex" -> str))),
    new CockpitListFilter("LoginName", str => Json.obj("loginName" -> Json.obj("$regex" -> str.toLowerCase))),
    new CockpitListFilter("Email", str => Json.obj("email" -> Json.obj("$regex" -> str))),
    new CockpitListFilter("PhoneNumber", str => Json.obj("phoneNumber" -> Json.obj("$regex" -> str)))
  )

  def evolutions = Map(
    0 -> AccountEvolutions.migrateToVerifiedString,
    1 -> AccountEvolutions.addDeviceIds,
    2 -> AccountEvolutions.convertToPushDevice,
    3 -> AccountEvolutions.removePushDevices,
    4 -> AccountEvolutions.addAccountProperties,
    5 -> AccountEvolutions.addUserSettings
  )
}

case class AccountReservation(loginName: String,
                              id: MongoId,
                              created: Date) {

  def toJson: JsObject = {
    Json.obj("loginName" -> this.loginName) ++
      Json.obj("reservationSecret" -> this.id.toString)
  }
}

object AccountReservation extends Model[AccountReservation] {

  implicit val col = reservedAccountCollection

  implicit val mongoFormat: Format[AccountReservation] = createMongoFormat(Json.reads[AccountReservation], Json.writes[AccountReservation])

  def evolutions: Map[Int, Reads[JsObject]] = Map()

  def reserve(loginName: String): Future[AccountReservation] = {
    val res = new AccountReservation(loginName, IdHelper.generateReservationSecret(), new Date)
    col.insert(res).map {
      lastError => res
    }
  }

  def findByLoginName(loginName: String): Future[Option[AccountReservation]] = {
    val query = Json.obj("loginName" -> Json.obj("$regex" -> ("^" + loginName + "$"), "$options" -> "i"))
    col.find(query).one[AccountReservation]
  }

  def deleteReserved(loginName: String): Future[LastError] = {
    val query = Json.obj("loginName" -> Json.obj("$regex" -> ("^" + loginName + "$"), "$options" -> "i"))
    col.remove(query)
  }

  def createDefault(): AccountReservation = {
    new AccountReservation(IdHelper.randomString(8), IdHelper.generateMongoId(), new Date)
  }

  def checkReservationSecret(value: String, secret: String): Future[Boolean] = {
    // do not require reservation secret for test users
    val testUserPrefix = Play.configuration.getString("testUser.prefix").get
    value.startsWith(testUserPrefix) match {
      case true => Future(true)
      case false =>
        AccountReservation.findByLoginName(value).map {
          case None => false
          case Some(reservation) =>
            reservation.id.id.equals(secret) match {
              case false => false
              case true =>
                AccountReservation.deleteReserved(value)
                true
            }
        }
    }
  }
}

object AccountEvolutions {

  def migrateToVerifiedString: Reads[JsObject] = Reads {
    js =>
      // deleted migration. It was not needed and caused problems.
      val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(1)))
      js.transform(addVersion)
  }

  def addDeviceIds: Reads[JsObject] = Reads {
    js =>
      val addArray = __.json.update((__ \ 'deviceIds).json.put(JsArray()))
      val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(2)))
      js.transform(addArray andThen addVersion)
  }

  def convertToPushDevice: Reads[JsObject] = Reads {
    js =>
      val deleteArray = (__ \ 'deviceIds).json.prune
      val addArray = __.json.update((__ \ 'pushDevices).json.put(JsArray()))
      val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(3)))
      js.transform(deleteArray andThen addArray andThen addVersion)
  }

  def removePushDevices: Reads[JsObject] = Reads {
    js =>
      val deleteArray = (__ \ 'pushDevices).json.prune
      val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(4)))
      js.transform(deleteArray andThen addVersion)
  }

  def addAccountProperties: Reads[JsObject] = Reads {
    js =>
      val addProperties = __.json.update((__ \ 'properties).json.put(Json.toJson(AccountProperties.defaultProperties)))
      val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(5)))
      js.transform(addProperties andThen addVersion)
  }

  def addUserSettings: Reads[JsObject] = Reads {
    js =>
      val addSettings = __.json.update((__ \ 'userSettings).json.put(Json.toJson(AccountUserSettings.defaultSettings)))
      val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(6)))
      js.transform(addSettings andThen addVersion)
  }
}

object AccountModelUpdate extends ModelUpdate {
  def values = Seq(
    VerifiedStringUpdateValue("email", JsonHelper.verifyMail, externalEdit = true),
    VerifiedStringUpdateValue("phoneNumber", JsonHelper.verifyPhoneNumber, externalEdit = true),
    StringUpdateValue("password"),
    BooleanUpdateValue("registrationIncomplete", externalEdit = true),
    BooleanUpdateSubvalue("userSettings", "enableUnreadMessages", externalEdit = true),
    BooleanUpdateSubvalue("userSettings", "convertSmileysToEmojis", externalEdit = true),
    BooleanUpdateSubvalue("userSettings", "sendOnReturn", externalEdit = true),
    StringUpdateSubvalue("userSettings", "languageSettings", externalEdit = true),
    StringUpdateSubvalue("userSettings", "dateFormat", externalEdit = true),
    StringUpdateSubvalue("userSettings", "timeFormat", externalEdit = true)
  )
}

case class AccountProperties(fileQuota: Int)

object AccountProperties {
  implicit val format: Format[AccountProperties] = Json.format[AccountProperties]

  def defaultProperties: AccountProperties = {
    def defaultQuota = Play.configuration.getInt("accounts.properties.default.file.quota").getOrElse(10000) * 1024 * 1024
    AccountProperties(defaultQuota)
  }
}

case class AccountUserSettings(enableUnreadMessages: Boolean,
                               convertSmileysToEmojis: Boolean,
                               sendOnReturn: Boolean,
                               languageSettings: String,
                               dateFormat: String,
                               timeFormat: String)

object AccountUserSettings {
  implicit val format: Format[AccountUserSettings] = Json.format[AccountUserSettings]

  def defaultSettings: AccountUserSettings =
    AccountUserSettings(enableUnreadMessages = true, convertSmileysToEmojis = true, sendOnReturn = false, "", "dd.MM.yyyy", "HH:mm")
}