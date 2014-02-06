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
import helper.MongoHelper._

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

  def filter(offset: Int = 0, varLimit: Int = 0) = AuthAction.async(parse.tolerantJson) {
    request =>

      // set default limit
      val limit = if (varLimit < 1) 150 else varLimit

      request.body.validate[FilterRules].map {
        fr =>
          {
            def collectIdentities(ids: Option[Seq[String]], groups: Option[Seq[String]]): Seq[MongoId] = {
              ids.getOrElse(Seq()).map(s => new MongoId(s)) ++
                groups.getOrElse(Seq()).flatMap {
                  group =>
                    request.identity.getGroup(group).map { _.identityId }
                }
            }

            // create pipeline
            val conversations = request.identity.conversations
            val from: Seq[MongoId] = collectIdentities(fr.fromIdentities, fr.fromGroups)
            val to: Seq[MongoId] = collectIdentities(fr.toIdentities, fr.toGroups)

            def createMatch(ids: Seq[MongoId], matchFunction: (MongoId => JsObject)): Match = {
              if (ids.size < 1) {
                Match(toBson(Json.obj()).get)
              }
              else {
                val matchJson = Json.obj("$or" -> ids.map(id => Json.obj("_id" -> id)))
                Match(toBson(matchJson).get)
              }
            }

            val pipeline: Seq[PipelineOperator] = Seq(
              createMatch(conversations, id => Json.obj("_id" -> id)),
              Unwind("messages"),
              Project(("messages", BSONInteger(1)), ("_id", BSONInteger(-1))),
              createMatch(from, id => Json.obj("messages.fromIdentityId" -> id)),
              createMatch(to, id => Json.obj("messages.messageStatus.identityId" -> id)),
              Sort(Seq(Ascending("messages.created"))),
              Skip(offset),
              Limit(limit)
            )

            val aggregationCommand = Aggregate(Conversation.col.name, pipeline)

            mongoDB.command(aggregationCommand).map {
              res =>
                val messages: Seq[JsValue] = res.force.toList.map(Json.toJson(_))
                resOK(messages.map(js => (js \ "messages").as[Message].toJson))
            }
          }
      }.recoverTotal(error => Future.successful(BadRequest(resKO(JsError.toFlatJson(error)))))
  }
}
