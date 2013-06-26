package models

import java.util.Date
import traits.{ModelHelper, MongoHelper}
import play.api.libs.json._
import play.api.libs.functional.syntax._
import helper.IdHelper

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 1:29 PM
 */

case class Conversation(
                         conversationId: String,
                         created: Date,
                         lastUpdated: Date,
                         recipients: Seq[Recipient],
                         messages: Seq[Message]
                         )

object Conversation extends MongoHelper with ModelHelper{

  implicit val mongoFormat = createMongoFormat(Json.reads[Conversation], Json.writes[Conversation])

  val inputReads = Json.reads[Conversation]

  val outputWrites = Writes[Conversation] {
    conversation =>
      Json.obj("conversationId" -> conversation.conversationId) ++
        Json.obj("recipients" -> toSortedArray[Recipient](conversation.recipients, Recipient.outputWrites, Recipient.sortWith)) ++
        Json.obj("messages" -> toSortedArray[Message](conversation.messages, Message.outputWrites, Message.sortWith)) ++
        Json.obj("created" -> defaultDateFormat.format(conversation.created)) ++
        addCreated(conversation.created) ++
        addLastUpdated(conversation.lastUpdated)
  }

}
