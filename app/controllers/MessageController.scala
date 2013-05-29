package controllers

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import play.api.mvc.{Action, Controller}
import play.modules.reactivemongo.MongoController
import helper.{IdHelper, MongoHelper}
import play.api.libs.json.JsArray
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.modules.reactivemongo.json.collection.JSONCollection
import scala.None
import scala.concurrent.Future


/**
 * User: Björn Reimer
 * Date: 5/21/13
 * Time: 7:08 PM
 */


object MessageController extends MongoHelper {

  /**
   * JSON Transformers
   */

  val addMessageId: Reads[JsObject] = __.json.update((__ \ 'messageId).json.put(JsString(IdHelper.generateMessageId())))
  val addConversationId: Reads[JsObject] = (__ \ 'conversationId).json.put(JsString(IdHelper.generateConversationId()))

  // Recipient Validator
  val validateRecipient: Reads[JsObject] = (
    (__ \ 'messageType).json.pickBranch(Reads.of[JsString]) and
      (__ \ 'sendTo).json.pickBranch(Reads.of[JsString]) and
      ((__ \ 'Name).json.pickBranch(Reads.of[JsString]) or emptyObj)
    ).reduce

  // Message validator
  val validateMessage: Reads[JsObject] = (
    (__ \ 'messageBody).json.pickBranch(Reads.of[JsString]) and
      ((__ \ 'conversationId).json.pickBranch(Reads.of[JsString]) or addConversationId) and
      ((__ \ 'assets).json.pickBranch(Reads.of[JsArray] keepAnd Reads.list(Reads.of[JsString])) or emptyObj) and
      (__ \ 'recipients).json.pickBranch(Reads.of[JsArray] keepAnd Reads.list(validateRecipient))
    ).reduce

  // Send Message result
  val sendMessageResult: Reads[JsObject] = (
    (__ \ 'conversationId).json.pickBranch and
      (__ \ 'messageId).json.pickBranch
    ).reduce

  // Returned Message
  def outputMessage(messageId: String): Reads[JsObject] = (
    (__).json.copyFrom((__ \ 'messages \ messageId).json.pick[JsObject]) andThen
      fromCreated andThen
      (__ \ 'password).json.prune
    )

  // returned conversation
  val outputConversation: Reads[JsObject] = (
    fromCreated andThen
      (__ \ '_id).json.prune /*andThen
      (__ \ 'messages \\ 'created).json.prune*/
    )


  // create conversation from single message
  def createConversation(messageId: String): Reads[JsObject] = (
    (__ \ 'messages \ messageId).json.copyFrom((__).json.pick[JsObject]) and
      (__ \ 'conversationId).json.pickBranch
    ).reduce

  // create mongodb update query that adds the message to the messages object
  def toConversationUpdateQuery(messageId: String): Reads[JsObject] = {
    (__ \ '$set \ {
      "messages." + messageId
    }).json.copyFrom((__).json.pick[JsObject])
  }

  /**
   * Future Actors TODO convert to Akka actor
   */

  def addMessageToConversation(message: JsObject): Option[String] = {

    val conversationId: String = (message \ "conversationId").as[String]
    val messageId: String = (message \ "messageId").as[String]

    // check if this conversationId exists
    val futureCollection = conversationCollection.find(Json.obj("conversationId" -> conversationId)).one[JsObject]

    futureCollection.map {
      case None => {
        // conversation does not exist yet, create new
        message.transform(createConversation(messageId) andThen addObjectIdAndDate).map {
          jsRes => conversationCollection.insert(jsRes).map {
            lastError =>
              InternalServerError(resKO("MongoError: " + lastError))
          }
        }.recoverTotal(
          error => InternalServerError(resKO(JsError.toFlatJson(error)))
        )
        //res = Some("Created new conversation: " + conversationId)
      }
      case Some(c: JsValue) => {
        // conversation does exist, add message to it
        message.transform(toConversationUpdateQuery(messageId)).map {
          jsUpdate => {
            conversationCollection.update(Json.obj("conversationId" -> conversationId), jsUpdate).map {
              lastError => InternalServerError(resKO("MongoError: " + lastError))
            }

          }
        }
        //res = Some("Added message to conversation: " + conversationId)
      }
    }
    Some("Sending not yet implemented")
  }

  /**
   * Helper
   */
  def findMessage(messageId: String, coll: JSONCollection): Future[Option[JsObject]] = {
    coll.find(Json.obj({"messages." + messageId} -> Json.obj {"$exists" -> true}), Json.obj({"messages." + messageId} -> true)).one[JsObject]
  }

  /**
   * Actions
   */

  def sendMessage = AuthenticateToken(requireAdminRight = false) {
    request =>
      val jsBody: JsValue = request.body

      jsBody.transform(validateMessage andThen addCreateDate andThen addMessageId).map {
        jsRes => {
          addMessageToConversation(jsRes) match {
            case Some(m) => Ok(resOK(jsRes.transform(sendMessageResult andThen addStatus(m)).get))
            case None => InternalServerError("Error")
          }
        }
      }.recoverTotal(
        error => BadRequest(resKO(JsError.toFlatJson(error)))
      )
  }

  def getMessage(messageId: String) = Action {
    request =>
      Async {
        findMessage(messageId, conversationCollection).map {
          case Some(m: JsObject) => m.transform(outputMessage(messageId) andThen addStatus("pending to send")).map {
            jsRes => Ok(resOK(jsRes))
          }.recoverTotal {
            error => InternalServerError(resKO(JsError.toFlatJson(error)))
          }
          case None => NotFound(resKO("Message not found: " + messageId))
        }
      }
  }

  def getConversation(conversationId: String) = Action {
    request =>
      Async {
        val futureConversation = conversationCollection.find(Json.obj("conversationId" -> conversationId)).one[JsObject]

        futureConversation.map {
          case Some(c: JsObject) => c.transform(outputConversation).map {
            jsRes => Ok(resOK(jsRes))
          }.recoverTotal {
            error => InternalServerError(resKO(JsError.toFlatJson(error)))
          }
          case None => NotFound(resKO("Conversation not found: " + conversationId))
        }
      }
  }

}
