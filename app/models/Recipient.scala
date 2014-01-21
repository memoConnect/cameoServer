package models

import play.api.libs.json._
import play.api.libs.functional.syntax._
import helper.IdHelper
import traits.{Model}
import scala.concurrent.{ExecutionContext, Future}
import reactivemongo.core.commands.LastError
import play.api.Logger
import ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 2:35 PM
 */
case class Recipient(
                      recipientId: String,
                      name: String,
                      messageType: String,
                      sendTo: String,
                      sendStatus: Option[String],
                      testRun: Option[Boolean]
                      )  {
  def toJson:JsValue = Json.toJson(this)(Recipient.outputWrites)

}

object Recipient extends Model[Recipient] {

  implicit val col = userCollection
  implicit val mongoFormat: Format[Recipient] = createMongoFormat(Json.reads[Recipient], Json.writes[Recipient])

  def createReads = (
    Reads.pure[String](IdHelper.generateRecipientId()) and
      (__ \ 'name).read[String] and
      (__ \ 'messageType).read[String] and
      (__ \ 'sendTo).read[String] and
      Reads.pure(None) and
      (__ \ 'test).readNullable[Boolean]
    )(Recipient.apply _)

  def outputWrites = Writes[Recipient] {
    r =>
      Json.obj("recipientId" -> r.recipientId) ++
        Json.obj("name" -> r.name) ++
        Json.obj("messageType" -> r.messageType) ++
        Json.obj("sendTo" -> r.sendTo) ++
        toJsonOrEmpty("sendStatus", r.sendStatus)
  }

  Json.writes[Recipient]

  override val sortWith = {
    (r1: Recipient, r2: Recipient) => r1.name < r2.name
  }

  /*
   * Helper
   */

//  def updateStatus(message: Message, recipient: Recipient, newStatus: String): Future[Option[String]] = {
//
//    val res = for {
//    // TODO: find a trick to do this without the message position
//      messagePosition <- models.Message.getMessagePosition(message.conversationId.getOrElse(""),
//        message.messageId)
//      lastError <- {
//        val query = Json.obj("conversationId" -> message.conversationId) ++ Json.obj("messages." +
//          messagePosition +
//          ".recipients.recipientId" -> recipient.recipientId)
//        val set = Json.obj("$set" -> Json.obj("messages." + messagePosition + ".recipients.$.sendStatus" ->
//          newStatus))
//
//        conversationCollection.update(query, set)
//      }
//    } yield lastError
//
//    res.map {
//      case (lastError: LastError) =>
//        if (lastError.inError) {
//          val error = "Error updating recipient status"
//          Logger.error(error)
//          Some(error)
//        }
//        else if (!lastError.updatedExisting) {
//          val error = "Nothing updated"
//          Logger.error(error)
//          Some(error)
//        } else {
//          None
//        }
//    }
//  }
}
