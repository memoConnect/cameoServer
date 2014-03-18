package helper

import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import play.modules.reactivemongo.json.collection.JSONCollection

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import java.text.SimpleDateFormat
import java.util.{ Date, TimeZone }
import reactivemongo.api.indexes.{ IndexType, Index }
import reactivemongo.bson.BSONDocument
import play.modules.reactivemongo.json.BSONFormats
import models.{ MongoId, VerifiedString }
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import org.mindrot.jbcrypt.BCrypt
import play.api.Logger
import play.api.libs.json.JsArray
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import scala.Some
import play.api.libs.json.JsNumber

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
      __.json.update((__ \ 'lastUpdated \ '$date).json.copyFrom((__ \ 'lastUpdated).json.pick[JsNumber]) or emptyObj)
  }

  val fromMongoDates: Reads[JsObject] = {
    __.json.update((__ \ 'created).json.copyFrom((__ \ 'created \ '$date).json.pick[JsNumber]) or emptyObj) andThen
      __.json.update((__ \ 'lastUpdated).json.copyFrom((__ \ 'lastUpdated \ '$date).json.pick[JsNumber]) or emptyObj)
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
