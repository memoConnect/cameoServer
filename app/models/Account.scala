package models

import java.util.Date

import helper.JsonHelper._
import helper.MongoCollections._
import helper.{ IdHelper, JsonHelper }
import models.cockpit.CockpitListFilter
import models.cockpit.attributes._
import play.api.Play
import play.api.Play.current
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import reactivemongo.core.commands.LastError
import traits._

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
                   created: Date,
                   lastUpdated: Date) {

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

  def createReads(): Reads[Account] = {
    val id = IdHelper.generateAccountId()
    (Reads.pure[MongoId](id) and
      (__ \ 'loginName).read[String] and
      (__ \ 'password).read[String](minLength[String](8) andKeep hashPassword) and
      (__ \ 'phoneNumber).readNullable[VerifiedString](verifyPhoneNumber andThen VerifiedString.createReads) and
      (__ \ 'email).readNullable[VerifiedString](verifyMail andThen VerifiedString.createReads) and
      Reads.pure[AccountProperties](AccountProperties.defaultProperties) and
      Reads.pure[Date](new Date()) and
      Reads.pure[Date](new Date()))(Account.apply _)
  }

  def outputWrites: Writes[Account] = Writes {
    a =>
      Json.obj("id" -> a.id.toJson) ++
        Json.obj("loginName" -> a.loginName) ++
        maybeEmptyJsValue("phoneNumber", a.phoneNumber.map(_.toJson)) ++
        maybeEmptyJsValue("email", a.email.map(_.toJson)) ++
        addCreated(a.created) ++
        addLastUpdated(a.lastUpdated)
  }

  def findByLoginName(loginName: String): Future[Option[Account]] = {
    val query = Json.obj("loginName" -> loginName.toLowerCase)
    find(query)
  }

  def createDefault(): Account = {
    new Account(IdHelper.generateAccountId(), IdHelper.randomString(8), "", None, None, AccountProperties.defaultProperties, new Date, new Date)
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

  def docVersion = 5

  def evolutions = Map(
    0 -> AccountEvolutions.migrateToVerifiedString,
    1 -> AccountEvolutions.addDeviceIds,
    2 -> AccountEvolutions.convertToPushDevice,
    3 -> AccountEvolutions.removePushDevices,
    4 -> AccountEvolutions.addAccountProperties
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

  def docVersion: Int = 0

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
      val movePhoneNumber = __.json.update((__ \ 'phoneNumber \ 'value).json.copyFrom((__ \ 'phoneNumber).json.pick[JsString]))
      val pnAddVerified = __.json.update((__ \ 'phoneNumber \ 'isVerified).json.put(JsBoolean(false)))
      val pnAddDate = __.json.update((__ \ 'phoneNumber \ 'lastUpdated).json.put(Json.obj("$date" -> new Date())))
      val phoneNumber: Reads[JsObject] =
        (js \ "phoneNumber").asOpt[String] match {
          case None    => Reads { js => JsSuccess(js.as[JsObject]) } // do nothing
          case Some(n) => movePhoneNumber andThen pnAddVerified andThen pnAddDate
        }

      val moveMail = __.json.update((__ \ 'email \ 'value).json.copyFrom((__ \ 'email).json.pick[JsString]))
      val mAddVerified = __.json.update((__ \ 'email \ 'isVerified).json.put(JsBoolean(false)))
      val mAddDate = __.json.update((__ \ 'email \ 'lastUpdated).json.put(Json.obj("$date" -> new Date())))
      val email =
        (js \ "email").asOpt[String] match {
          case None    => Reads { js => JsSuccess(js.as[JsObject]) } // do nothing
          case Some(m) => moveMail andThen mAddVerified andThen mAddDate
        }

      val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(1)))

      js.transform(phoneNumber andThen email andThen addVersion)
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
}

object AccountUpdate extends ModelUpdate {
  def values = Seq(
    VerifiedStringUpdateValue("email", JsonHelper.verifyMail, externalEdit = true),
    VerifiedStringUpdateValue("phoneNumber", JsonHelper.verifyPhoneNumber, externalEdit = true),
    StringUpdateValue("password")
  )
}

case class AccountProperties(fileQuota: Int)

object AccountProperties {
  implicit def format: Format[AccountProperties] = Json.format[AccountProperties]

  def defaultProperties: AccountProperties = {
    def defaultQuota = Play.configuration.getInt("accounts.properties.default.file.quota").getOrElse(10000) * 1024 * 1024
    AccountProperties(defaultQuota)
  }
}