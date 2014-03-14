package models

import play.api.libs.json.{Json, Format}

/**
 * User: Björn Reimer
 * Date: 2/4/14
 * Time: 10:07 AM
 */
// assumes that all data is verified
case class SmsMessage(from: String,
                      to: String,
                      body: String)

object SmsMessage {
  implicit val format: Format[SmsMessage] = Json.format[SmsMessage]
}
