package controllers

import traits.ExtendedController
import models._
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._
import helper.OutputLimits
import helper.CmActions.AuthAction
import play.api.libs.json._
import helper.ResultHelper._

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 7:51 PM
 */
object ConversationController extends ExtendedController {

  case class CreateConversationRequest(subject: Option[String])
  object CreateConversationRequest { implicit val format = Json.format[CreateConversationRequest] }

  def createConversation = AuthAction().async(parse.tolerantJson) {
    request =>
      {
        validateFuture[CreateConversationRequest](request.body, CreateConversationRequest.format) {
          ccr =>
            val conversation = Conversation.create(ccr.subject, Seq(Recipient.create(request.identity.id)))
            Conversation.col.insert(conversation).map {
              le => resOK(conversation.toJson)
            }
        }
      }
  }

  def getConversation(id: String, offset: Int, limit: Int, maybeKeyId: Option[String]) = AuthAction(allowExternal = true).async {
    request =>
      Conversation.find(id, limit, offset).map {
        case None => resNotFound("conversation")
        case Some(c) => c.hasMemberResult(request.identity.id) {
          maybeKeyId match {
            case None        => resOK(c.toJson)
            case Some(keyId) => resOK(c.toJsonWithKey(keyId))
          }
        }
      }
  }

  def addRecipients(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>

      Conversation.find(new MongoId(id), -1, 0).flatMap {
        case None => Future.successful(resNotFound("conversation"))
        case Some(c) =>

          c.hasMemberFutureResult(request.identity.id) {

            validateFuture[Seq[String]](request.body \ "recipients", Reads.seq[String]) {
              recipientIds =>
                {
                  // check if one of the identities is already a member of this conversation
                  recipientIds.forall(id => !c.hasMember(new MongoId(id))) match {
                    case false => Future(resKO("At least one identity is already a member of this conversation"))
                    case true =>

                      // check if all recipients are in the users address book
                      recipientIds.forall(recipient => request.identity.contacts.exists(_.identityId.id.equals(recipient))) match {
                        case false => Future(resKO("At least one identity is not a contact"))
                        case true =>

                          // check if all recipients exist
                          val maybeIdentities = Future.sequence(recipientIds.map(Identity.find))
                          val futureResult: Future[Boolean] = maybeIdentities.map {
                            i => i.forall(_.isDefined)
                          }
                          futureResult.flatMap {
                            case false => Future(resBadRequest("At least one identityId is invalid"))
                            case true =>
                              c.addRecipients(recipientIds.map(Recipient.create)).map {
                                case true  => resOK("updated")
                                case false => resServerError("update failed")
                              }
                          }
                      }
                  }
                }
            }
          }
      }
  }

  def deleteRecipient(id: String, rid: String) = AuthAction().async {
    request =>
      Conversation.find(id, -1, 0).flatMap {
        case None => Future(resNotFound("conversation"))
        case Some(c) => c.hasMemberFutureResult(request.identity.id) {
          c.deleteRecipient(new MongoId(rid)).map {
            case false => resNotFound("recipient")
            case true  => resOK()
          }
        }
      }
  }

  def getConversationSummary(id: String) = AuthAction(allowExternal = true).async {
    request =>
      Conversation.find(id, -1, 0).flatMap {
        case None => Future(resNotFound("conversation"))
        case Some(c) => c.hasMemberFutureResult(request.identity.id) {
          c.toSummaryJson.map(resOK(_))
        }
      }
  }

  def getConversations(offset: Int, limit: Int) = AuthAction().async {
    request =>
      Conversation.findByIdentityId(request.identity.id).flatMap {
        list =>
          // TODO: this can be done more efficiently with the aggregation framework in mongo
          val sorted = list.sortBy(_.lastUpdated).reverse
          val limited = OutputLimits.applyLimits(sorted, offset, limit)
          val futureJson = Future.sequence(limited.map(_.toSummaryJson))
          futureJson.map {
            json =>
              val res = Json.obj("conversations" -> json, "numberOfConversations" -> list.length)
              resOK(res)
          }
      }
  }

  def updateConversation(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, ConversationUpdate.createReads) {
        cu =>
          Conversation.find(id, -1, 0).flatMap {
            case None => Future(resNotFound("conversation"))
            case Some(c) => c.hasMemberFutureResult(request.identity.id) {
              c.update(cu).map {
                case false => resServerError("could not update")
                case true  => resOK("updated")
              }
            }
          }
      }
  }
}