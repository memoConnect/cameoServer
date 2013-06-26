package models

import play.api.libs.json._
import play.api.libs.functional.syntax._
import helper.IdHelper

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 2:35 PM
 */
case class Recipient(
                      recipientId: String,
                      name: String,
                      messageType: String,
                      sendTo: String
                      )

object Recipient {

  implicit val mongoFormat = Json.format[Recipient]

  val inputReads = (
    Reads.pure[String](IdHelper.generateRecipientId()) and
      (__ \ 'name).read[String] and
      (__ \ 'messageType).read[String] and
      (__ \ 'sendTo).read[String]
    )(Recipient.apply _)

  val outputWrites = Json.writes[Recipient]

  val sortWith = {
    (r1: Recipient, r2: Recipient) => r1.name < r2.name
  }
}
