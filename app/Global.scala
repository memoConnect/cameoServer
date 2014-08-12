/**
 * User: Björn Reimer
 * Date: 5/25/13
 * Time: 4:27 PM
 */
import helper.DbAdminUtilities
import helper.MongoCollections._
import models.{ Conversation, GlobalState }
import play.api.Play.current
import play.api.http.HeaderNames._
import play.api.libs.json.{ JsValue, Json }
import play.api.mvc.{ EssentialAction, EssentialFilter, WithFilters }
import play.api.{ Logger, Play }
import play.modules.statsd.api.Statsd

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

object AccessControllFilter extends EssentialFilter {
  // wrap action to modify the headers of every request
  def apply(action: EssentialAction): EssentialAction = EssentialAction {
    request =>
      val accessControllEnabled = Play.configuration.getString("headers.accessControl.enable")
      accessControllEnabled match {
        case Some("true") =>
          action.apply(request).map(_.withHeaders(
            ACCESS_CONTROL_ALLOW_METHODS -> "GET, POST, DELETE, PUT, OPTIONS",
            ACCESS_CONTROL_ALLOW_ORIGIN -> "*",
            ACCESS_CONTROL_ALLOW_HEADERS -> "Authorization, Content-type, X-File-Name, X-Max-Chunks, X-File-Size, X-File-Type, X-Index, X-TwoFactorToken")
          )
        case _ => action.apply(request)
      }
  }
}

object Global extends WithFilters(new play.modules.statsd.api.StatsdFilter(), AccessControllFilter) {

  override def onStart(app: play.api.Application) = {
    // make sure that we have a connection to mongodb
    def checkMongoConnection(): Boolean =
      {
        try {
          val futureState = conversationCollection.find(Json.obj()).one[Conversation].map(_.getOrElse(Json.obj()))
          Await.result(futureState, 1.minute)
          Logger.info("DB Connection OK")
          true
        } catch {
          case e: Exception =>
            Logger.error("Could not connect to mongodb", e)
            Thread.sleep(1000)
            checkMongoConnection()
        }
      }

    checkMongoConnection()

    // load fixtures
    if (Play.configuration.getString("mongo.init.loadOnStart").getOrElse("fail").equalsIgnoreCase("true")) {

      // only load if there is no data in the DB already
      val futureRes: Future[Boolean] = conversationCollection.find(Json.obj()).one[JsValue].flatMap {
        case None =>
          Logger.info("Loading initial data")
          DbAdminUtilities.loadFixtures()
        case Some(i) => Future(true)
      }

      Await.result(futureRes, 5.minutes)
      Logger.debug("finished loading fixtures")

    }

    // global database migrations
    if (Play.configuration.getString("mongo.migrate.global").getOrElse("fail").equalsIgnoreCase("true")) {

      def migrate: Future[Boolean] = {
        val latestVersion: Int = DbAdminUtilities.latestDbVersion

        // get global state from db
        val futureState: Future[GlobalState] = globalStateCollection.find(Json.obj()).one[GlobalState].flatMap {
          case None =>
            val newState = new GlobalState(0, false)
            globalStateCollection.insert(newState).map(lastError => newState)
          case Some(s) => Future(s)
        }

        val state = Await.result(futureState, 1.minute)

        state match {
          // only migrate if there is a new version and nobody else is migrating
          case GlobalState(version, false) if version < latestVersion =>
            Logger.info("Migrating. Current Version: " + version + " latestVersion: " + latestVersion)
            // apply migrations
            DbAdminUtilities.migrate(version)
          case GlobalState(version, false) if version == latestVersion =>
            Logger.debug("Global db version: " + version + ". no migrations required")
            Future(true)
          case GlobalState(version, false) if version > latestVersion =>
            Logger.error("Global db version (" + version + ") higher than latest version in code: " + latestVersion)
            Future(false)
          case GlobalState(version, true) =>
            // wait until lock is lifted
            Logger.info("A global migration seems to be running, waiting...")
            Thread.sleep(2000)
            migrate
          case _ =>
            Logger.error("no global db version, This should not happen ;)")
            Future(false)
        }
      }

      Await.result(migrate, 6.hours)

      Statsd.increment("custom.instances")
    }

  }

  override def onStop(app: play.api.Application) = {
    Statsd.increment("custom.instances",-1)
  }
}

