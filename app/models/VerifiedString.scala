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


case class VerifiedString(
                           isVerified: Boolean,
                           string: String,
                           lastUpdated: Date
                           )  {
  override def toString: String = string
}

object VerifiedString extends MongoHelper {
  implicit val mongoFormat: Format[VerifiedString] = createMongoFormat(Json.reads[VerifiedString], Json.writes[VerifiedString])

  val createReads: Reads[VerifiedString] = (
    Reads.pure[Boolean](false) and
      __.read[String] and
      Reads.pure[Date](new Date)
    )(VerifiedString.apply _)

  def create(string: String): VerifiedString = {
    new VerifiedString(false, string, new Date)
  }

  def createOpt(string: Option[String]): Option[VerifiedString] = {

    string match {
      case None => None
      case Some(s) => Some(new VerifiedString(false, s, new Date))
    }

  }
}
