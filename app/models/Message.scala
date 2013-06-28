package models

import java.util.Date
import traits.{Model, MongoHelper}
import play.api.libs.json._
import helper.IdHelper
import play.api.libs.functional.syntax._

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 2:36 PM
 */
case class Message(
                    messageId: String,
                    conversationId: Option[String],
                    messageBody: String,
                    from: String,
                    created: Date,
                    recipients: Option[Seq[Recipient]]
                    )


object Message extends MongoHelper with Model[Message] {

  implicit val collection = conversationCollection
  implicit val mongoFormat: Format[Message] = createMongoFormat(Json.reads[Message], Json.writes[Message])


  val inputReads = (
    Reads.pure[String](IdHelper.generateMessageId()) and
      (__ \ 'conversationId).readNullable[String] and
      (__ \ 'messageBody).read[String] and
      Reads.pure[String]("") and
      Reads.pure[Date](new Date) and
      (__ \ 'recipients).readNullable[Seq[Recipient]](Reads.seq(Recipient.inputReads))
    )(Message.apply _)

  val outputWrites = Writes[Message] {
    m =>
      Json.obj("messageId" -> m.messageId) ++
        Json.obj("conversationId" -> JsString(m.conversationId.getOrElse("none"))) ++
        Json.obj("messageBody" -> m.messageBody) ++
        Json.obj("from" -> m.from) ++
        Json.obj("recipients" -> Recipient.toSortedArray(m.recipients.getOrElse(Seq()))) ++
        addCreated(m.created)
  }

  override val sortWith = {
    (m1: Message, m2: Message) => m1.created.before(m2.created)
  }
}

