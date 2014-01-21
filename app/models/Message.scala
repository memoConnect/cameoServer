package models

import java.util.Date
import traits.{Model, MongoHelper}
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
                    from: String,                           // Name to be displayed next to Message
                    fromRecipientId: Option[String],                // Recipient Id of the sender
                    created: Date,
                    recipients: Option[Seq[Recipient]],
                    assets: Option[Seq[Asset]]
                    )      {
  def toJson:JsValue = Json.toJson(this)(Message.outputWrites)

}


object Message extends MongoHelper with Model[Message] {

  //implicit val col = conversationCollection
  implicit val mongoFormat: Format[Message] = createMongoFormat(Json.reads[Message], Json.writes[Message])

  def createReads = (
    Reads.pure[String](IdHelper.generateMessageId()) and
      (__ \ 'conversationId).readNullable[String] and
      (__ \ 'messageBody).read[String] and
      Reads.pure[String]("") and
      Reads.pure(None) and
      Reads.pure[Date](new Date) and
      (__ \ 'recipients).readNullable[Seq[Recipient]](Reads.seq(Recipient.createReads)) and
      Reads.pure(None)
    )(Message.apply _)

  def outputWrites = Writes[Message] {
    m =>
      Json.obj("messageId" -> m.messageId) ++
        Json.obj("conversationId" -> JsString(m.conversationId.getOrElse("none"))) ++
        Json.obj("messageBody" -> m.messageBody) ++
        Json.obj("from" -> m.from) ++
//        Recipient.toSortedJsonObjectOrEmpty("recipients", m.recipients)(imits(0,0)) ++
//        Asset.toSortedJsonObjectOrEmpty("assets", m.assets) ++
        addCreated(m.created)
  }



//  def find(messageId: String, conversationId: String): Future[Option[Message]] = {
//    find(Json.obj("conversationId" -> conversationId, "messages.messageId" -> messageId))
//  }

  def find(messageId: String): Future[Option[Message]] = {
    Conversation.col.find(Json.obj("messages.messageId" -> messageId)).one[Message]
  }
//
//  def find(query: JsObject): Future[Option[Message]] = {
//    val filter = Json.obj("messages.$" -> 1)
//    col.find(query, filter).cursor[JsObject].collect[List](1000, stopOnError = false).map {
//      list =>
//        list.size match {
//          case 0 => None
//          case 1 => {
//            (list(0) \ "messages")(0).validate[Message].map {
//              message =>
//                if (message.messageId.equals((query \ "messageId").asOpt[String].getOrElse(message.messageId))) {
//                  Some(message)
//                } else {
//                  Logger.error("Received message with wrong Id: " + message.messageId + " Query: " + query.toString)
//                  None
//                }
//            }.recoverTotal{
//              e => None
//            }
//          }
//          case _ => {
//            Logger.error("CRITICAL: MessageId is not unique! query: " + query.toString)
//            None
//          }
//        }
//    }
//  }

  override val sortWith = {
    (m1: Message, m2: Message) => m1.created.before(m2.created)
  }

  // gets the position of a message in a conversation
//  def getMessagePosition(conversationId: String, messageId: String): Future[Int] ={
//
//    val query = Json.obj("conversationId" -> conversationId)
//
//    conversationCollection.find(query).one[Conversation].map{
//      case None => -1
//      case Some(c) => c.messages.indexWhere(m => {m.messageId.equals(messageId)})
//    }
//
//  }
}

