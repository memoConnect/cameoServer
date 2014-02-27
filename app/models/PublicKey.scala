package models

import play.api.libs.json.{Json, Format}

/**
 * User: Björn Reimer
 * Date: 2/27/14
 * Time: 11:19 AM
 */
case class PublicKey(name: Option[String], key: String)

object PublicKey {

  implicit val format: Format[PublicKey] = Json.format[PublicKey]

}
