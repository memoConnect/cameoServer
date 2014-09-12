package actors

import akka.actor.Actor
import com.amazonaws.services.elastictranscoder.model.Pipeline
import helper.JsonHelper._
import helper.MongoCollections
import models.{Account, Conversation}
import play.api.Logger
import play.api.libs.json.Json
import play.modules.statsd.Statsd
import reactivemongo.bson.{BSONNull, BSONDocument}
import reactivemongo.core.commands._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.modules.reactivemongo.json.BSONFormats._

/**
 * User: Björn Reimer
 * Date: 12.09.14
 * Time: 12:41
 */

case object MessageCount
case object AccountCount

class StatsActor extends Actor {

  def receive = {

    case MessageCount =>

        val pipeline: Seq[PipelineOperator] = Seq(
          Match(BSONDocument()),
          Unwind("messages"),
          Group(BSONNull)(("count", SumValue(1))))

        val command = Aggregate(Conversation.col.name, pipeline)

        MongoCollections.mongoDB.command(command).map {
          _.headOption match {
              case None => Logger.error("Could not get message count")
              case Some(bson) =>
                val count = (Json.toJson(bson) \ "count").as[Int]
//                Logger.debug("MessageCount: " + count)
                Statsd.gauge("messages.total", count)
            }
        }

    case AccountCount =>

      val query = BSONDocument()
      val command = Count(Account.col.name, Some(query))

      MongoCollections.mongoDB.command(command).map {
        count =>
//          Logger.debug("AccountCount: " + count)
          Statsd.gauge("accounts.total", count)
      }


  }

}
