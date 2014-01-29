package controllers

import play.api.libs.json._

import helper.AuthAction
import traits.ExtendedController
import models._
import helper.ResultHelper._
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global


/**
 * User: Björn Reimer
 * Date: 5/21/13
 * Time: 7:08 PM
 */
object MessageController extends ExtendedController {

  /**
   * Helper
   */

  /**
   * Actions
   */

  def createMessage(id: String) = AuthAction.async(parse.tolerantJson) {
    request =>
      request.body.validate[Message](Message.createReads(request.identity.id)).map {
        message => {
          Conversation.find(new MongoId(id)).map {
            case None => NotFound(resKO("invalid id"))
            case Some(conversation) => {
              conversation.addMessage(message)
              //actors.sendMessageActor ! (message, conversation.recipients)
              resOK(message.toJson)
            }
          }
        }
      }.recoverTotal(error => Future.successful(BadRequest(resKO(JsError.toFlatJson(error)))))
  }


    def getMessage(id: String) = AuthAction.async {
      (request) =>
        Message.find(new MongoId(id)).map {
          case None => NotFound(resKO("message not found"))
          case Some(m) => resOK(m.toJson)
        }
    }


}
