package traits

import play.api.libs.json._
import play.api.libs.json.Reads._
import java.text.SimpleDateFormat
import org.mindrot.jbcrypt.BCrypt
import java.util.{TimeZone, Date}
import play.modules.reactivemongo.json.collection.JSONCollection
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 6:46 PM
 */
//case class OutputLimits(offset: Int, limit: Int)

trait Model[A] extends MongoHelper {

  //val col: JSONCollection
  implicit val mongoFormat: Format[A]

  //def createReads: Reads[A]
  //def outputWrites(implicit ol: OutputLimits): Writes[A]

  val sortWith = (o1: A, o2: A) => true


  /**
   * Helper
   */
//  def toJson(model: A)(implicit ol: OutputLimits = OutputLimits(0, 0)): JsValue = {
//    Json.toJson[A](model)(outputWrites)
//  }

//  def toJsonCustomWrites(model: A, writes: Writes[A])(implicit ol: OutputLimits = OutputLimits(0, 0)): JsValue = {
//    Json.toJson[A](model)(writes)
//  }

  def toJsonOrEmpty(key: String, value: Option[String]): JsObject = {
    value match {
      case Some(s) => Json.obj(key -> JsString(s))
      case None => Json.obj()
    }
  }

  def toJsonArrayOrEmpty(key: String, value: Option[Seq[String]]): JsObject = {
    value match {
      case Some(s) => Json.obj(key -> JsArray(s.map(JsString)))
      case None => Json.obj()
    }
  }

//  def toSortedJsonArray(array: Seq[A])(implicit ol: OutputLimits  = OutputLimits(0,0)): JsArray = toSortedJsonArray(array, outputWrites)
//
//  def toSortedJsonArray(array: Seq[A], writes: Writes[A])(implicit ol: OutputLimits): JsArray = {
//    val sorted = array.sortWith(sortWith).map(Json.toJson[A](_)(writes))
//
//    def mustBePositive(i: Int) = if (i < 0) {
//      0
//    } else {
//      i
//    }
//    val start = mustBePositive(math.min(ol.offset, sorted.size - 1))
//    val end = mustBePositive(
//      ol.limit match {
//        case 0 => sorted.size
//        case _ => math.min(start + ol.limit, sorted.size)
//      })
//
//    val subset = sorted.slice(start, end)
//    JsArray(subset)
//
//  }
//
//  def toSortedJsonObject(key: String, array: Seq[A])(implicit ol: OutputLimits = OutputLimits(0,0)): JsObject = {
//    Json.obj(key -> toSortedJsonArray(array))
//  }
//
//  def toSortedJsonObjectOrEmpty(key: String, array: Option[Seq[A]])(implicit ol: OutputLimits = OutputLimits(0,0)): JsObject = {
//    array match {
//      case Some(a) => toSortedJsonObject(key, a)
//      case None => Json.obj()
//    }
//  }
//
//  // get Array from Document
//  def getArray(queryKey: String, queryValue: String, arrayKey: String): Future[Option[Seq[A]]] = {
//    val query = Json.obj(queryKey -> queryValue)
//    val filter = Json.obj(arrayKey -> 1)
//
//    col.find(query, filter).one[JsObject].map {
//      case None => None
//      case Some(js: JsObject) => Some((js \ arrayKey).asOpt[Seq[A]](Reads.seq[A]).getOrElse(Seq()))
//    }
//  }

  val defaultDateFormat: SimpleDateFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss")
  defaultDateFormat.setTimeZone(TimeZone.getTimeZone("Europe/Berlin"))

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
