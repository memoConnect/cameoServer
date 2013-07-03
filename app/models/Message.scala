package models

import java.util.Date
import traits.{OutputLimits, Model, MongoHelper}
import play.api.libs.json._
import helper.IdHelper
import play.api.libs.functional.syntax._
import scala.concurrent.{ExecutionContext, Future}
import play.api.Logger
import ExecutionContext.Implicits.global

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
                    recipients: Option[Seq[Recipient]],
                    assets: Option[Seq[Asset]]
                    )


object Message extends MongoHelper with Model[Message] {

  implicit val collection = conversationCollection
  implicit val mongoFormat: Format[Message] = createMongoFormat(Json.reads[Message], Json.writes[Message])


  def inputReads = (
    Reads.pure[String](IdHelper.generateMessageId()) and
      (__ \ 'conversationId).readNullable[String] and
      (__ \ 'messageBody).read[String] and
      Reads.pure[String]("") and
      Reads.pure[Date](new Date) and
      (__ \ 'recipients).readNullable[Seq[Recipient]](Reads.seq(Recipient.inputReads)) and
      Reads.pure(None)
    )(Message.apply _)

  def outputWrites(implicit ol: OutputLimits = OutputLimits(0,0)) = Writes[Message] {
    m =>
      Json.obj("messageId" -> m.messageId) ++
        Json.obj("conversationId" -> JsString(m.conversationId.getOrElse("none"))) ++
        Json.obj("messageBody" -> m.messageBody) ++
        Json.obj("from" -> m.from) ++
        Recipient.toSortedJsonObjectOrEmpty("recipients", m.recipients)(OutputLimits(0,0)) ++
        Asset.toSortedJsonObjectOrEmpty("assets", m.assets) ++
        addCreated(m.created)
  }

  def find(messageId: String, conversationId: String): Future[Option[Message]] = {
    find(Json.obj("conversationId" -> conversationId, "messages.messageId" -> messageId))
  }

  def find(messageId: String): Future[Option[Message]] = {
    find(Json.obj("messages.messageId" -> messageId))
  }

  def find(query: JsObject): Future[Option[Message]] = {
    val filter = Json.obj("messages.$" -> 1)
    collection.find(query, filter).cursor[JsObject].toList.map {
      list =>
        list.size match {
          case 0 => None
          case 1 => {
            (list(0) \ "messages")(0).validate[Message].map {
              message =>
                if (message.messageId.equals((query \ "messageId").asOpt[String].getOrElse(message.messageId))) {
                  Some(message)
                } else {
                  Logger.error("Received message with wrong Id: " + message.messageId + " Query: " + query.toString)
                  None
                }
            }.recoverTotal{
              e => None
            }
          }
          case _ => {
            Logger.error("CRITICAL: MessageId is not unique! query: " + query.toString)
            None
          }
        }
    }
  }

  override val sortWith = {
    (m1: Message, m2: Message) => m1.created.before(m2.created)
  }

  // gets the position of a message in a conversation
  def getMessagePosition(conversationId: String, messageId: String): Future[Int] ={

    val query = Json.obj("conversationId" -> conversationId)

    conversationCollection.find(query).one[Conversation].map{
      case None => -1
      case Some(c) => c.messages.indexWhere(m => {Logger.debug(m.messageId); m.messageId.equals(messageId)})
    }

  }
}

