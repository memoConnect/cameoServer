package controllers

import traits.ExtendedController
import models.{ Identity, MongoId, Conversation }
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._
import helper.AuthAction
import play.api.libs.json._
import helper.ResultHelper._
import scala.Some

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 7:51 PM
 */
object ConversationController extends ExtendedController {

  def createConversation = AuthAction(parse.tolerantJson) {
    request =>
      {
        validate[Conversation](request.body, Conversation.createReads) {
          c =>
            {
              // add creator of conversation to recipients
              val withCreator = c.copy(recipients = c.recipients :+ request.identity.id)
              Conversation.col.insert(withCreator)
              request.identity.addConversation(withCreator.id)
              resOK(withCreator.toJson)
            }
        }
      }
  }

  def getConversation(id: String, offset: Int, limit: Int) = AuthAction.async {
    request =>
      Conversation.find(new MongoId(id)).flatMap {
        case None => Future.successful(resNotFound("conversation"))
        case Some(c) => c.hasMember(request.identity.id) {
          c.toJsonWithDisplayNamesResult(offset, limit)
        }
      }
  }

  def addRecipients(id: String) = AuthAction.async(parse.tolerantJson) {
    request =>

      Conversation.find(new MongoId(id)).flatMap {
        case None => Future.successful(resNotFound("conversation"))
        case Some(c) => {

          c.hasMember(request.identity.id) {

            validateFuture[Seq[String]](request.body \ "recipients", Reads.seq[String]) {
              recipientIds =>
                {
                  // check if all recipients exist
                  val maybeIdentities = Future.sequence(recipientIds.map({
                    id => Identity.find(id)
                  }))
                  val futureResult: Future[Boolean] = maybeIdentities.map {
                    i => i.forall(_.isDefined)
                  }
                  futureResult.flatMap {

                    case false => Future(resBadRequest("at least one recipientId is invalid"))
                    case true => {

                      c.addRecipients(recipientIds.map(new MongoId(_))).map {
                        lastError =>
                          if (lastError.ok) {
                            resOK("updated")
                          }
                          else {
                            resServerError("update failed")
                          }
                      }
                    }
                  }
                }
            }
          }
        }
      }
  }

  def getConversationSummary(id: String) = AuthAction.async {
    request =>
      Conversation.find(id).flatMap {
        case None => Future(resNotFound("conversation"))
        case Some(c) => c.hasMember(request.identity.id) {
          Future(resOK(c.toSummaryJson))
        }
      }
  }

  def getConversations(offset: Int, limit: Int) = AuthAction.async {
    request =>

      val list: Seq[Future[JsObject]] = request.identity.conversations.map {
        c =>
          Conversation.find(c).map {
            case None               => Json.obj()
            case Some(conversation) => conversation.toSummaryJson
          }
      }

      Future.sequence(list).map { resOK(_) }
  }
}