package traits

import play.api.libs.json._
import play.api.libs.json.Reads._
import java.text.SimpleDateFormat
import org.mindrot.jbcrypt.BCrypt

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 6:46 PM
 */
trait ModelHelper {

  val defaultDateFormat: SimpleDateFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm")

  def toJsonOrEmpty(value: Option[String], key: String): JsObject = {
    value match {
      case Some(s) => Json.obj(key -> JsString(s))
      case None => Json.obj()
    }
  }

  def toJsonArrayOrEmpty(value: Option[Seq[String]], key: String): JsObject = {
    value match {
      case Some(s) => Json.obj(key -> JsArray(s.map(JsString(_))))
      case None => Json.obj()
    }
  }

  def toSortedArray[T](array: Seq[T], writes: Writes[T], sortBy: ((T) => String)): JsArray = {
    JsArray(array.sortBy(sortBy).map(Json.toJson[T](_)(writes)))
  }

  val hashPassword: Reads[String] = Reads[String] {
    js => js.asOpt[String] match {
      case None => JsError("No password")
      case Some(pass) => JsSuccess(BCrypt.hashpw(pass, BCrypt.gensalt()))
    }
  }


}
