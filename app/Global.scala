/**
 * User: Björn Reimer
 * Date: 5/25/13
 * Time: 4:27 PM
 */

import java.io.IOException

import de.flapdoodle.embed.mongo.{ MongodProcess, MongodExecutable, MongodStarter }
import de.flapdoodle.embed.mongo.config.{ Net, IMongodConfig, MongodConfigBuilder }
import de.flapdoodle.embed.mongo.distribution.Version
import de.flapdoodle.embed.mongo.tests.MongodForTestsFactory
import de.flapdoodle.embed.process.runtime.Network
import helper.MongoCollections._
import helper.{ DbAdminUtilities, MongoCollections }
import models.{ Conversation, GlobalState }
import play.api.Play.current
import play.api.http.HeaderNames._
import play.api.libs.json.{ JsValue, Json }
import play.api.mvc.{ EssentialAction, EssentialFilter, WithFilters }
import play.api.{ Logger, Play }
import play.modules.statsd.api.Statsd
import reactivemongo.bson.{ BSONDocument, BSONString, BSONValue }
import reactivemongo.core.commands._

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

object StatsFilter extends EssentialFilter {

  def apply(action: EssentialAction): EssentialAction = EssentialAction {
    request =>

      request.path match {
        case path if path.startsWith("/a/v1/") =>
          Statsd.increment("custom.request.api.combined")
          request.method match {
            case method if method.equalsIgnoreCase("GET")     => Statsd.increment("custom.request.api.GET")
            case method if method.equalsIgnoreCase("POST")    => Statsd.increment("custom.request.api.POST")
            case method if method.equalsIgnoreCase("PUT")     => Statsd.increment("custom.request.api.PUT")
            case method if method.equalsIgnoreCase("DELETE")  => Statsd.increment("custom.request.api.DELETE")
            case method if method.equalsIgnoreCase("OPTIONS") => Statsd.increment("custom.request.api.OPTIONS")
          }
        case path if path.startsWith("/m") => Statsd.increment("custom.request.m")
        case path if path.startsWith("/c") => Statsd.increment("custom.request.cockpit")
        case path if path.startsWith("/p") => Statsd.increment("custom.request.purl")
        case _                             =>
      }

      action.apply(request)
  }
}

object Global extends WithFilters(new play.modules.statsd.api.StatsdFilter(), AccessControllFilter, StatsFilter) {

  override def onStart(app: play.api.Application) = {

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
    Logger.info("Shutting down app")
    Statsd.increment("custom.instances", -1)
  }

  // make sure that we have a connection to mongodb
  private def checkMongoConnection(): Boolean = {

    // command to get the database version
    case class BuildInfo() extends Command[Map[String, BSONValue]] {
      override def makeDocuments = BSONDocument("buildinfo" -> 1)

      object ResultMaker extends BSONCommandResultMaker[Map[String, BSONValue]] {
        def apply(document: BSONDocument) = Right(document.elements.toMap)
      }
    }

    try {
      val conversationResult = conversationCollection.find(Json.obj()).one[Conversation].map(_.getOrElse(Json.obj()))
      Await.result(conversationResult, 1.minute)

      val futureBuildInfo = MongoCollections.mongoDB.command(BuildInfo())
      val buildInfo = Await.result(futureBuildInfo, 1.minute)
      val version = buildInfo.get("version") match {
        case Some(BSONString(str)) => str
        case _                     => "na"
      }
      Logger.info("DB Connection OK. Version: " + version)
      DbAdminUtilities.mongoVersion = version
      true
    } catch {
      case e: Exception =>
        Logger.error("Could not connect to mongodb", e)
        Thread.sleep(1000)
        checkMongoConnection()
    }
  }

  private def startLocalMongo(): Boolean = {

    val mongoPort = Play.configuration.getInt("embed.mongo.port").get
    val mongoVersion = Play.configuration.getString("embed.mongo.dbversion").get

    val starter: MongodStarter = MongodStarter.getDefaultInstance

    val mongodConfig: IMongodConfig = new MongodConfigBuilder()
      .version(new Version(mongoVersion))
      .net(new Net(mongoPort, Network.localhostIsIPv6()))
      .build()

    val mongodExecutable: MongodExecutable = starter.prepare(mongodConfig)
    val mongod: MongodProcess = mongodExecutable.start()

  }

}

