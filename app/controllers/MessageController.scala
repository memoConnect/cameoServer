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
import reactivemongo.bson._
import java.util.Date
import helper.MongoHelper._
import play.api.libs.functional.syntax._
import reactivemongo.core.commands.Group
import reactivemongo.core.commands.Sort
import reactivemongo.core.commands.Match
import reactivemongo.bson.BSONDateTime
import reactivemongo.bson.BSONString
import scala.Some
import reactivemongo.core.commands.SumValue
import reactivemongo.bson.BSONInteger
import reactivemongo.core.commands.Unwind
import reactivemongo.core.commands.Limit
import reactivemongo.core.commands.Project
import play.api.libs.json.JsObject
import reactivemongo.core.commands.Skip
import reactivemongo.core.commands.Ascending
import java.lang.NumberFormatException
import play.api.libs.concurrent.Akka
import akka.actor.Props
import actors.SendMessageActor
import play.api.Play.current

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
                // initiate new actor for this message
                val sendMessageActor = Akka.system.actorOf(Props[SendMessageActor])
                sendMessageActor ! (message, conversation.recipients)
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

    val dateReads: Reads[Date] = Reads {
      js =>
        js.asOpt[String] match {
          case None =>
            JsError()
          case Some(s) => {
            try {
              JsSuccess(new Date(s.toLong * 1000))
            }
            catch {
              case e: NumberFormatException => JsError()
            }
          }
        }
    }

    implicit val reads: Reads[FilterRules] = (
      (__ \ 'fromIdentities).readNullable[Seq[String]] and
      (__ \ 'toIdentities).readNullable[Seq[String]] and
      (__ \ 'fromGroups).readNullable[Seq[String]] and
      (__ \ 'fromGroups).readNullable[Seq[String]] and
      (__ \ 'startDate).readNullable[Date](dateReads) and
      (__ \ 'endDate).readNullable[Date](dateReads)
    )(FilterRules.apply _)

  }

  def filter(offset: Int = 0, varLimit: Int = 0, count: String = "false") = AuthAction.async(parse.tolerantJson) {
    request =>

      // set default limit
      val limit = if (varLimit < 1) 150 else varLimit

      implicit val dateReads: Reads[Date] = Reads.IsoDateReads

      request.body.validate[FilterRules].map {
        fr =>
          {
            def collectIdentities(ids: Option[Seq[String]], groups: Option[Seq[String]]): Seq[MongoId] = {
              ids.getOrElse(Seq()).map(s => new MongoId(s)) ++
                groups.getOrElse(Seq()).flatMap {
                  group =>
                    request.identity.getGroup(group).map {
                      _.identityId
                    }
                }
            }

            // create pipeline
            val conversations = request.identity.conversations
            val from: Seq[MongoId] = collectIdentities(fr.fromIdentities, fr.fromGroups)
            val to: Seq[MongoId] = collectIdentities(fr.toIdentities, fr.toGroups)
            //            val startDate: Date = new Date(fr.startDate)

            def createMatch(ids: Seq[MongoId], matchFunction: (MongoId => JsObject)): Match = {
              if (ids.size < 1) {
                Match(toBson(Json.obj()).get)
              }
              else {
                val matchJson = Json.obj("$or" -> ids.map(matchFunction))
                Match(toBson(matchJson).get)
              }
            }

            def createDateMatch(dateOpt: Option[Date], matchOperator: String): Seq[Match] = {
              dateOpt match {
                case None => Seq()
                case Some(date) => {
                  val d = BSONDateTime(date.getTime)
                  val doc = BSONDocument(("messages.created", BSONDocument((matchOperator, d))))
                  Seq(Match(doc))
                }
              }
            }

            val basePipeline: Seq[PipelineOperator] = Seq(
              createMatch(conversations, id => Json.obj("_id" -> id)),
              Unwind("messages"),
              Project(("messages", BSONInteger(1)), ("_id", BSONInteger(-1))),
              createMatch(from, id => Json.obj("messages.fromIdentityId" -> id)),
              createMatch(to, id => Json.obj("messages.messageStatus.identityId" -> id))) ++
              createDateMatch(fr.startDate, "$gt") ++
              createDateMatch(fr.endDate, "$lt")

            if (count.equalsIgnoreCase("true")) {
              val pipeline = basePipeline ++
                Seq(Group(BSONString("result"))(("count", SumValue(1))))

              val aggregationCommand = Aggregate(Conversation.col.name, pipeline)

              mongoDB.command(aggregationCommand).map {
                res =>

                  val result = res.force.toList.headOption

                  if (result.isEmpty) {
                    resOK("0")
                  }
                  else {
                    val count = (Json.toJson(result.get) \ "count").as[Int]
                    resOK(count.toString)
                  }
              }

            }
            else {
              val pipeline = basePipeline ++
                Seq(
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
          }
      }.recoverTotal(error => Future.successful(BadRequest(resKO(JsError.toFlatJson(error)))))
  }
}
