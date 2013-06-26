package models

import java.util.Date
import traits.{ModelHelper, MongoHelper}
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
                    sendStatus: JsObject,
                    recipients: Option[Seq[Recipient]]
                    )


object Message extends MongoHelper with ModelHelper {

  implicit val mongoFormat = createMongoFormat(Json.reads[Message], Json.writes[Message])

  val inputReads = (
    Reads.pure[String](IdHelper.generateMessageId()) and
      (__ \ 'conversationId).readNullable[String] and
      (__ \ 'messageBody).read[String] and
      Reads.pure[String]("") and
      Reads.pure[Date](new Date) and
      Reads.pure[JsObject](Json.obj()) and
      (__ \ 'recipients).readNullable[Seq[Recipient]](Reads.seq(Recipient.inputReads))
    )(Message.apply _)

  val outputWrites = Writes[Message] {
    m =>
      Json.obj("messageId" -> m.messageId) ++
        Json.obj("conversationId" -> JsString(m.conversationId.getOrElse("none"))) ++
        Json.obj("messageBody" -> m.messageBody) ++
        Json.obj("from" -> m.from) ++
        Json.obj("sendStatus" -> m.sendStatus) ++
        addCreated(m.created)

  }

  val sortWith = {
    (m1: Message, m2: Message) => m1.created.before(m2.created)
  }
}

