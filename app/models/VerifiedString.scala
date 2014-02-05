package models

import java.util.Date
import traits.MongoHelper

import play.api.libs.json._
import play.api.libs.functional.syntax._

/**
 * User: Björn Reimer
 * Date: 2/3/14
 * Time: 10:48 AM
 */

case class VerifiedString(isVerified: Boolean,
                          value: String,
                          lastUpdated: Date) {

  override def toString: String = value

  def toJson: JsObject = Json.toJson(this)(VerifiedString.outputWrites).as[JsObject]
}

object VerifiedString extends MongoHelper {

  implicit val mongoFormat: Format[VerifiedString] = createMongoFormat(Json.reads[VerifiedString], Json.writes[VerifiedString])

  val createReads: Reads[VerifiedString] = (
    Reads.pure[Boolean](false) and
    __.read[String] and
    Reads.pure[Date](new Date))(VerifiedString.apply _)

  val outputWrites: Writes[VerifiedString] = Writes {
    vs =>
      Json.obj(
        "value" -> vs.value,
        "isVerified" -> vs.isVerified)
  }

  def create(string: String): VerifiedString = {
    new VerifiedString(false, string, new Date)
  }

  def createOpt(string: Option[String]): Option[VerifiedString] = {

    string match {
      case None    => None
      case Some(s) => Some(new VerifiedString(false, s, new Date))
    }

  }
}
