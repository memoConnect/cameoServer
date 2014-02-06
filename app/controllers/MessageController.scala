package controllers

import play.api.libs.json._

import helper.AuthAction
import traits.ExtendedController
import models._
import helper.ResultHelper._
import scala.concurrent.{ ExecutionContext, Future }
import ExecutionContext.Implicits.global
import reactivemongo.core.commands._
import play.modules.reactivemongo.json.BSONFormats._
import play.api.Logger
import reactivemongo.bson.{ BSONInteger, BSON, BSONValue }
import reactivemongo.api.SortOrder
import java.util.Date

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
        message =>
          {
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
        case None    => NotFound(resKO("message not found"))
        case Some(m) => resOK(m.toJson)
      }
  }

  case class FilterRules(fromIdentities: Option[Seq[String]],
                         toIdentities: Option[Seq[String]],
                         fromGroups: Option[Seq[String]],
                         toGroups: Option[Seq[String]],
                         startDate: Option[Date],
                         endDate: Option[Date])

  object FilterRules {
    implicit val format: Format[FilterRules] = createMongoFormat(Json.reads[FilterRules], Json.writes[FilterRules])
  }

  def filter() = AuthAction.async(parse.tolerantJson) {
    request =>

      request.body.validate[FilterRules].map {
        fr => {
          // create pipeline
          val from = fr.fromIdentities.getOrElse(Seq) ++ Contact.
        }



      }

      // first get all conversations of this user
      val matchConversations = Json.obj("$or" -> request.identity.conversations.map(c => Json.obj("_id" -> Json.toJson(c))))

      val pipeline: Seq[PipelineOperator] = Seq(
        Match(toBson(matchConversations).get),
        Unwind("messages"),
        Project(("messages", BSONInteger(1)), ("_id", BSONInteger(-1))),
        Sort(Seq(Ascending("messages.created"))) //,
      //        Limit(10)
      )

      val aggregationCommand = Aggregate(Conversation.col.name, pipeline)

      mongoDB.command(aggregationCommand).map {
        res =>

          val messages: Seq[JsValue] = res.force.toList.map(Json.toJson(_))

          Ok(messages.toString())
      }
  }
}
