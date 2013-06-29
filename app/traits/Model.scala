package traits

import play.api.libs.json._
import play.api.libs.json.Reads._
import java.text.SimpleDateFormat
import org.mindrot.jbcrypt.BCrypt
import java.util.Date
import play.modules.reactivemongo.json.collection.JSONCollection
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 6:46 PM
 */
trait Model[A] extends MongoHelper {

  implicit val collection: JSONCollection
  implicit val mongoFormat: Format[A]

  val inputReads: Reads[A]
  val outputWrites: Writes[A]

  val sortWith = (o1: A, o2: A) => true

  /**
   * Helper
   */

  def toJson(model: A): JsValue = {
    Json.toJson[A](model)(outputWrites)
  }

  def toJsonOrEmpty(key: String, value: Option[String]): JsObject = {
    value match {
      case Some(s) => Json.obj(key -> JsString(s))
      case None => Json.obj()
    }
  }

  def toJsonArrayOrEmpty(key: String, value: Option[Seq[String]]): JsObject = {
    value match {
      case Some(s) => Json.obj(key -> JsArray(s.map(JsString(_))))
      case None => Json.obj()
    }
  }

  def toSortedJsonArray(key: String, array: Seq[A]): JsObject = {
    Json.obj(key -> JsArray(array.sortWith(sortWith).map(Json.toJson[A](_)(outputWrites))))
  }

  def toSortedJsonArray(array: Seq[A]): JsArray = {
    JsArray(array.sortWith(sortWith).map(Json.toJson[A](_)(outputWrites)))
  }

  def toSortedJsonArrayOrEmpty(key: String, array: Option[Seq[A]]): JsObject = {
    array match {
      case Some(a) => Json.obj(key -> JsArray(a.sortWith(sortWith).map(Json.toJson[A](_)(outputWrites))))
      case None => Json.obj()
    }
  }



  // get Array from Document
  def getArray(queryKey: String, queryValue: String, arrayKey: String): Future[Option[Seq[A]]] = {
    val query = Json.obj(queryKey -> queryValue)
    val filter = Json.obj(arrayKey -> 1)

    collection.find(query, filter).one[JsObject].map {
      case None => None
      case Some(js: JsObject) => Some((js \ arrayKey).asOpt[Seq[A]](Reads.seq[A]).getOrElse(Seq()))
    }
  }

  val defaultDateFormat: SimpleDateFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm")

  def addCreated(date: Date): JsObject = {
    Json.obj("created" -> defaultDateFormat.format(date))
  }

  def addLastUpdated(date: Date): JsObject = {
    Json.obj("lastUpdated" -> defaultDateFormat.format(date))
  }

  val hashPassword: Reads[String] = Reads[String] {
    js => js.asOpt[String] match {
      case None => JsError("No password")
      case Some(pass) => JsSuccess(BCrypt.hashpw(pass, BCrypt.gensalt()))
    }
  }
}
