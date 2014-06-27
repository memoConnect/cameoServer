package controllers

import java.util.Date

import actors.{ SendMessage, SendMessageActor }
import akka.actor.Props
import helper.CmActions.AuthAction
import helper.ResultHelper._
import models._
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.functional.syntax._
import play.api.libs.json._
import traits.ExtendedController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ ExecutionContext, Future }

/**
 * User: Björn Reimer
 * Date: 5/21/13
 * Time: 7:08 PM
 */
object MessageController extends ExtendedController {

  /**
   * Actions
   */
  def createMessage(id: String) = AuthAction(allowExternal = true).async(parse.tolerantJson) {
    request =>
      validateFuture[Message](request.body, Message.createReads(request.identity.id)) {
        message =>
          {
            Conversation.find(id, -1, 0).flatMap {
              case None => Future(resNotFound("conversation"))
              case Some(conversation) =>
                // only members can add message to conversation
                conversation.hasMemberFutureResult(request.identity.id) {
                  conversation.addMessage(message)
                  // initiate new actor for each request
                  val sendMessageActor = Akka.system.actorOf(Props[SendMessageActor])
                  sendMessageActor ! SendMessage(message, conversation.id, conversation.recipients, conversation.subject.getOrElse(""))
                  Future(resOk(message.toJson))
                }
            }
          }
      }
  }

  def getMessage(id: String) = AuthAction(allowExternal = true).async {
    (request) =>
      Message.findConversation(new MongoId(id)).map {
        case None => resNotFound("message")
        case Some(c) =>
          c.hasMemberResult(request.identity.id) {
            c.getMessage(new MongoId(id)) match {
              case None    => resServerError("unable to get message from conversation")
              case Some(m) => resOk(m.toJson)
            }
          }
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
            } catch {
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

  def filter(offset: Int = 0, varLimit: Int = 0, count: String = "false") = AuthAction().async(parse.tolerantJson) {
    request => Future(Ok(""))

    //      // set default limit
    //      val limit = if (varLimit < 1) 150 else varLimit
    //      val mongoDB = ReactiveMongoPlugin.db
    //
    //      implicit val dateReads: Reads[Date] = Reads.IsoDateReads
    //
    //      validateFuture[FilterRules](request.body, FilterRules.reads) {
    //        fr =>
    //          {
    //            def collectIdentities(ids: Option[Seq[String]], groups: Option[Seq[String]]): Seq[MongoId] = {
    //              ids.getOrElse(Seq()).map(s => new MongoId(s)) ++
    //                groups.getOrElse(Seq()).flatMap {
    //                  group =>
    //                    request.identity.getGroup(group).map {
    //                      _.identityId
    //                    }
    //                }
    //            }
    //
    //            // create pipeline
    //            val conversations = Conversation.findByIdentityId(request.identity.id).map{_.map{_.id}}
    //            val from: Seq[MongoId] = collectIdentities(fr.fromIdentities, fr.fromGroups)
    //            val to: Seq[MongoId] = collectIdentities(fr.toIdentities, fr.toGroups)
    //            //            val startDate: Date = new Date(fr.startDate)
    //
    //            def createMatch(ids: Seq[MongoId], matchFunction: (MongoId => JsObject)): Match = {
    //              if (ids.size < 1) {
    //                Match(toBson(Json.obj()).get)
    //              } else {
    //                val matchJson = Json.obj("$or" -> ids.map(matchFunction))
    //                Match(toBson(matchJson).get)
    //              }
    //            }
    //
    //            def createDateMatch(dateOpt: Option[Date], matchOperator: String): Seq[Match] = {
    //              dateOpt match {
    //                case None => Seq()
    //                case Some(date) => {
    //                  val d = BSONDateTime(date.getTime)
    //                  val doc = BSONDocument(("messages.created", BSONDocument((matchOperator, d))))
    //                  Seq(Match(doc))
    //                }
    //              }
    //            }
    //
    //            val basePipeline: Seq[PipelineOperator] = Seq(
    //              createMatch(conversations, id => Json.obj("_id" -> id)),
    //              Unwind("messages"),
    //              Project(("messages", BSONInteger(1)), ("_id", BSONInteger(-1))),
    //              createMatch(from, id => Json.obj("messages.fromIdentityId" -> id)),
    //              createMatch(to, id => Json.obj("messages.messageStatus.identityId" -> id))) ++
    //              createDateMatch(fr.startDate, "$gt") ++
    //              createDateMatch(fr.endDate, "$lt")
    //
    //            if (count.equalsIgnoreCase("true")) {
    //              val pipeline = basePipeline ++
    //                Seq(Group(BSONString("result"))(("count", SumValue(1))))
    //
    //              val aggregationCommand = Aggregate(Conversation.col.name, pipeline)
    //
    //              mongoDB.command(aggregationCommand).map {
    //                res =>
    //
    //                  val result = res.force.toList.headOption
    //
    //                  if (result.isEmpty) {
    //                    resOK("0")
    //                  } else {
    //                    val count = (Json.toJson(result.get) \ "count").as[Int]
    //                    resOK(count.toString)
    //                  }
    //              }
    //
    //            } else {
    //              val pipeline = basePipeline ++
    //                Seq(
    //                  Sort(Seq(Ascending("messages.created"))),
    //                  Skip(offset),
    //                  Limit(limit)
    //                )
    //
    //              val aggregationCommand = Aggregate(Conversation.col.name, pipeline)
    //
    //              mongoDB.command(aggregationCommand).map {
    //                res =>
    //                  val messages: Seq[JsValue] = res.force.toList.map(Json.toJson(_))
    //                  resOK(messages.map(js => (js \ "messages").as[Message].toJson))
    //              }
    //            }
    //          }
    //      }
  }
}

