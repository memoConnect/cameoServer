/**
 * User: Björn Reimer
 * Date: 5/25/13
 * Time: 4:27 PM
 */

import helper.DbAdminUtilities
import models.{ GlobalState, Account }
import play.api.libs.json.{ JsValue, Json }
import play.api.{ Logger, Play, GlobalSettings }
import info.schleichardt.play.embed.mongo.DynamicEmbedMongoPort
import play.api.mvc.EssentialAction
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.api.indexes.Index
import reactivemongo.api.MongoDriver
import scala.collection.JavaConverters._
import play.api.http.HeaderNames._
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future, ExecutionContext }
import ExecutionContext.Implicits.global
import play.api.Play.current
import helper.JsonHelper._

object Global extends GlobalSettings with DynamicEmbedMongoPort {

  // tell reactive mongo the port of the memory database created by embed mongo
  override def additionalEmbedMongoPortSettings(port: Int) = Map("mongodb.servers" -> List(s"localhost:$port").asJava)

  // wrap action to modify the headers of every request
  override def doFilter(action: EssentialAction): EssentialAction = EssentialAction {
    request =>
      action.apply(request).map(_.withHeaders(ACCESS_CONTROL_ALLOW_METHODS -> "GET, POST, DELETE, PUT, OPTIONS",
        ACCESS_CONTROL_ALLOW_ORIGIN -> "*", ACCESS_CONTROL_ALLOW_HEADERS -> "Authorization, Content-type"))
  }

  //  override def beforeStart(app: play.api.Application) = {
  //

  //  }

  override def onStart(app: play.api.Application) = {

    // load fixtures
    if (Play.configuration.getString("mongo.init.loadOnStart").getOrElse("fail").equalsIgnoreCase("true")) {
      // only load if there is no data in the DB alread
      val futureRes: Future[Boolean] = conversationCollection.find(Json.obj()).one[JsValue].flatMap {
        case None =>
          Logger.info("Loading initial data")
          DbAdminUtilities.loadFixtures()
        case Some(i) => Future(true)
      }

      Await.result(futureRes, 5 minutes)
      Logger.debug("finished loading fixtures")

    }

    // global database migrations
    if (Play.configuration.getString("mongo.migrate.global").getOrElse("fail").equalsIgnoreCase("true")) {

      val latestVersion: Int = DbAdminUtilities.latestDbVersion

      // get global state from db
      val state: Future[GlobalState] = globalStateCollection.find(Json.obj()).one[GlobalState].flatMap {
        case None =>
          val newState = new GlobalState(0, false)
          globalStateCollection.insert(newState).map(lastError => newState)
        case Some(s) => Future(s)
      }

      val res: Future[Boolean] = state.flatMap {
        // only migrate if there is a new version and nobody else is migrating
        case GlobalState(version, false) if version < latestVersion => {
          Logger.debug("Migrating. Current Version: " + version + " latestVersion: " + latestVersion)
          // apply migrations
          DbAdminUtilities.migrate(version)
        }
        case _ => Future(false)
      }

      Await.result(res, 1 hour)
    }

  }
}
