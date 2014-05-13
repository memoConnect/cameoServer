package helper

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import java.util.Date
import reactivemongo.bson.BSONDocument
import play.modules.reactivemongo.json.BSONFormats
import models.{ MongoId, VerifiedString }
import org.mindrot.jbcrypt.BCrypt
import play.api.libs.json.JsArray
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import scala.Some
import play.api.libs.json.JsNumber
import play.api.Logger

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 7:10 PM
 */
object JsonHelper {

  val emptyObj = __.json.put(Json.obj())

  // converts dates to mongo format ($date)
  val toMongoDates: Reads[JsObject] = {
    __.json.update((__ \ 'created \ '$date).json.copyFrom((__ \ 'created).json.pick[JsNumber]) or emptyObj) andThen
      __.json.update((__ \ 'lastUpdated \ '$date).json.copyFrom((__ \ 'lastUpdated).json.pick[JsNumber]) or emptyObj) andThen
      __.json.update((__ \ 'lastAccessed \ '$date).json.copyFrom((__ \ 'lastAccessed).json.pick[JsNumber]) or emptyObj)
  }

  val fromMongoDates: Reads[JsObject] = {
    __.json.update((__ \ 'created).json.copyFrom((__ \ 'created \ '$date).json.pick[JsNumber]) or emptyObj) andThen
      __.json.update((__ \ 'lastUpdated).json.copyFrom((__ \ 'lastUpdated \ '$date).json.pick[JsNumber]) or emptyObj) andThen
      __.json.update((__ \ 'lastAccessed).json.copyFrom((__ \ 'lastAccessed \ '$date).json.pick[JsNumber]) or emptyObj)
  }

  // converts id to _id
  val toMongoId: Reads[JsObject] = {
    __.json.update((__ \ '_id).json.copyFrom((__ \ 'id).json.pick[JsValue]) or emptyObj) andThen
      (__ \ 'id).json.prune
  }

  val fromMongoId: Reads[JsObject] = {
    __.json.update((__ \ 'id).json.copyFrom((__ \ '_id).json.pick[JsValue]) or emptyObj) andThen
      (__ \ '_id).json.prune
  }

  def addCreated(date: Date): JsObject = {
    Json.obj("created" -> PrintDate.toString(date))
  }

  def addLastUpdated(date: Date): JsObject = {
    Json.obj("lastUpdated" -> PrintDate.toString(date))
  }

  def maybeEmptyString(key: String, value: Option[String]): JsObject = {
    value match {
      case Some(s) => Json.obj(key -> JsString(s))
      case None    => Json.obj()
    }
  }

  def maybeEmptyInt(key: String, value: Option[Int]): JsObject = {
    value match {
      case Some(s) => Json.obj(key -> JsNumber(s))
      case None    => Json.obj()
    }
  }

  def maybeEmptySeq(key: String, value: Option[Seq[String]]): JsObject = {
    value match {
      case Some(s) => Json.obj(key -> JsArray(s.map(JsString)))
      case None    => Json.obj()
    }
  }

  def maybeEmptyJsValue(key: String, value: Option[JsValue]): JsObject = {
    value match {
      case Some(s) => Json.obj(key -> s)
      case None    => Json.obj()
    }
  }

  def getNewValueVerifiedString(old: Option[VerifiedString], newValue: VerifiedString): Option[VerifiedString] = {
    if (old.isDefined && old.get.value.equals(newValue.value)) {
      None
    } else {
      Some(newValue)
    }
  }

  def getNewValueString(old: Option[String], newValue: String): Option[String] = {
    if (old.isDefined && old.get.equals(newValue)) {
      None
    } else {
      Some(newValue)
    }
  }

  def toBson(json: JsValue): Option[BSONDocument] = {
    BSONFormats.toBSON(json).asOpt.map(_.asInstanceOf[BSONDocument])
  }

  val hashPassword: Reads[String] = Reads[String] {
    js =>
      js.asOpt[String] match {
        case None => JsError("No password")
        case Some(pass) => JsSuccess({
          val hashed = BCrypt.hashpw(pass, BCrypt.gensalt())
          hashed
        })
      }
  }

  def arrayQuery(arrayName: String, id: MongoId): JsObject = Json.obj(arrayName -> Json.obj("$elemMatch" -> Json.obj("_id" -> id)))

  def limitArray(name: String, limit: Int, offset: Int): JsObject = {

    // this is not very elegant, but there seems to be no way to get offset without limit in mongodb...
    def infiniteLimit = 100000

    (limit, offset) match {
      // no restrictions
      case (0, 0) => Json.obj()
      // offset only
      case (0, _) => Json.obj(name -> Json.obj("$slice" -> Seq(offset, infiniteLimit)))
      // limit only
      case (_ , 0) => Json.obj(name -> Json.obj("$slice" -> limit))
      // offset and limit
      case (_ , _) => Json.obj(name -> Json.obj("$slice" -> Seq(offset, limit)))
    }
  }

  def verifyMail: Reads[JsString] = Reads[JsString] {
    js =>
      js.validate[String].flatMap {
        mail =>
          CheckHelper.checkAndCleanEmailAddress(mail) match {
            case None          => JsError("invalid email: " + mail)
            case Some(checked) => JsSuccess(JsString(checked))
          }
      }
  }

  def verifyPhoneNumber: Reads[JsString] = Reads[JsString] {
    js =>
      js.validate[String].flatMap {
        phoneNumber =>
          CheckHelper.checkAndCleanPhoneNumber(phoneNumber) match {
            case None          => JsError("invalid phoneNumber: " + phoneNumber)
            case Some(checked) => JsSuccess(JsString(checked))
          }
      }
  }
}
