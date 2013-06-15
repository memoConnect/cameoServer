package controllers

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import helper.IdHelper
import scala.None
import scala.concurrent.Future
import play.api.mvc.Result
import traits.ExtendedController
import play.api.Logger
import java.io.{InputStream, File}
import reactivemongo.bson.BSONValue
import reactivemongo.api.gridfs.GridFS
import play.api.libs.iteratee.Enumerator


/**
 * User: Björn Reimer
 * Date: 5/21/13
 * Time: 7:08 PM
 */
object MessageController extends ExtendedController {

  /**
   * JSON Transformers
   */
  val addMessageId: Reads[JsObject] = __.json.update((__ \ 'messageId).json.put(JsString(IdHelper.generateMessageId())))
  val addConversationId: Reads[JsObject] = (__ \ 'conversationId).json.put(JsString(IdHelper.generateConversationId()))

  // Recipient Validator
  val validateRecipient: Reads[JsObject] = ((__ \ 'messageType).json.pickBranch(Reads.of[JsString]) and
    (__ \ 'sendTo).json.pickBranch(Reads.of[JsString]) and
    ((__ \ 'Name).json.pickBranch(Reads.of[JsString]) or emptyObj)).reduce

  // Assest validator
  val validateAsset: Reads[JsObject] = ((__ \ 'name).json.pickBranch(Reads.of[JsString]) and
    (__ \ 'size).json.pickBranch(Reads.of[JsString]) and
    (__ \ 'type).json.pickBranch(Reads.of[JsString]) and
    (__ \ 'content).json.pickBranch(Reads.of[JsString])).reduce

  // Message validator
  val validateMessage: Reads[JsObject] = ((__ \ 'messageBody).json.pickBranch(Reads.of[JsString]) and
    ((__ \ 'conversationId).json.pickBranch(Reads.of[JsString]) or addConversationId) and
    //(__ \ 'assets).json.pickBranch(Reads.of[JsArray] keepAnd Reads.list(validateAsset)) and
    (__ \ 'assets).json.put(Json.obj()) and
    (__ \ 'recipients).json.pickBranch(Reads.of[JsArray] keepAnd Reads.list(validateRecipient))).reduce

  // Add username to message
  def addUsername(username: String): Reads[JsObject] = __.json.update((__ \ 'username).json.put(JsString(username)))

  // Send Message result
  val sendMessageResult: Reads[JsObject] = ((__ \ 'conversationId).json.pickBranch and
    (__ \ 'messageId).json.pickBranch).reduce

  // Returned Message
  val outputMessage: Reads[JsObject] =
    fromCreated andThen
      createArrayFromIdObject("recipients", Reads(j => JsSuccess(j.as[JsObject]))) andThen
      createArrayFromIdObject("assets", Reads(j => JsSuccess(j.as[JsObject])) )

  // returned conversation
  val outputConversation: Reads[JsObject] =
    fromCreated andThen
      (__ \ '_id).json.prune andThen
      createArrayFromIdObject("messages", outputMessage)

  // create conversation from single message
  def createConversation(messageId: String): Reads[JsObject] = ((__ \ 'messages \ messageId).json.copyFrom(__.json
    .pick[JsObject]) and
    (__ \ 'conversationId).json.pickBranch).reduce

  // create mongodb update query that adds the message to the messages object in the conversation
  def addToConversationUpdateQuery(messageId: String): Reads[JsObject] = {
    (__ \ '$set \ {
      "messages." + messageId
    }).json.copyFrom(__.json.pick[JsObject])
  }

  /**
   * Helper
   */
  def addMessageOk(message: JsObject): Result = {
    sendMessageActor ! message
    Ok(resOK(message.transform(sendMessageResult).getOrElse(JsString("Unable to create result"))))
  }

  def addMessageToConversation(message: JsObject): Future[Result] = {
    val messageId: String = (message \ "messageId").as[String]

    // first save assets TODO: this is bad and memory intensive!
//    val assets: Seq[JsValue] = message.transform((__ \ 'assets).json.pick[JsArray]).getOrElse({
//      Logger.debug("Empty Assets"); JsArray()
//    }).value
//
//    assets.map {
//      asset: JsValue => {
//        // save file to gridFS
//        new sun.misc.BASE64Decoder().decodeBufferToByteBuffer((asset \ "content").as[String].substring(22))
//      }
//    }

    // check if this conversationId exists
    val futureCollection = conversationCollection.find(getConversationId(message)).one[JsObject]

    futureCollection.map {
      case None => {
        // conversation does not exist yet, create new
        message.transform(createConversation(messageId) andThen addObjectIdAndDate).map {
          jsRes => Async {
            conversationCollection.insert(jsRes).map {
              lastError => {
                if (lastError.ok) {
                  addMessageOk(message)
                } else {
                  InternalServerError(resKO("MongoError: " + lastError))
                }
              }
            }
          }
        }.recoverTotal(error => InternalServerError(resKO(JsError.toFlatJson(error))))
      }
      case Some(c: JsValue) => {
        // conversation does exist, add message to it
        message.transform(addToConversationUpdateQuery(messageId)).map {
          jsUpdate => Async {
            conversationCollection.update(getConversationId(message), jsUpdate).map {
              lastError => if (lastError.ok) {
                addMessageOk(message)
              } else {
                InternalServerError(resKO("MongoError: " + lastError))
              }
            }
          }
        }.recoverTotal(error => InternalServerError(resKO(JsError.toFlatJson(error))))
      }
    }
  }



  /**
   * Actions
   */
  def sendMessage = authenticatePOST(maxLength = 5 * 1024 * 1024) {
    (username, request) =>
      val jsBody: JsValue = request.body
      Async {
        val contentResult = jsBody.transform(validateMessage andThen
          addCreateDate andThen
          addMessageId andThen
          addUsername(username) andThen
          createIdObjectFromArray("recipients", IdHelper.generateRecipientId)).map {
          jsRes => addMessageToConversation(jsRes)
        }.recoverTotal(error => Future(BadRequest(resKO(JsError.toFlatJson(error)))))

        contentResult
      }
  }

  def getMessage(messageId: String, token: String) = authenticateGET(token) {
    (username, request) =>
      Async {
        findMessage(messageId).map {
          case Some(m: JsObject) =>
            m.transform(__.json.copyFrom((__ \ 'messages \ messageId).json.pick[JsObject]) andThen outputMessage).map {
              jsRes => Ok(resOK(jsRes))
            }.recoverTotal {
              error => InternalServerError(resKO(JsError.toFlatJson(error)))
            }
          case None => NotFound(resKO("Message not found: " + messageId))
        }
      }
  }

  def getConversation(conversationId: String, token: String) = authenticateGET(token) {
    (username, request) =>
      Async {
        val futureConversation = conversationCollection.find(Json.obj("conversationId" -> conversationId)).one[JsObject]
        futureConversation.map {
          case None => NotFound(resKO("Conversation not found: " + conversationId))
          case Some(c: JsObject) => c.transform(outputConversation).map {
            jsRes => Ok(resOK(jsRes))
          }.recoverTotal {
            error => InternalServerError(resKO(JsError.toFlatJson(error)))
          }
        }
      }
  }

}
