/**
 * User: Björn Reimer
 * Date: 5/25/13
 * Time: 4:27 PM
 */

import helper.DbAdminUtilities
import models.Account
import play.api.libs.json.{ JsValue, Json }
import play.api.{ Logger, Play, GlobalSettings }
import info.schleichardt.play.embed.mongo.DynamicEmbedMongoPort
import play.api.mvc.EssentialAction
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.api.indexes.Index
import reactivemongo.api.MongoDriver
import scala.collection.JavaConverters._
import play.api.http.HeaderNames._
import scala.concurrent.ExecutionContext
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

  override def onStart(app: play.api.Application) = {
    if (Play.configuration.getString("mongo.init.loadOnStart").getOrElse("fail").equalsIgnoreCase("true")) {

      // only load if there is no data in the DB alread
      conversationCollection.find(Json.obj()).one[JsValue].map {
        case None =>
          Logger.info("Loading initial data")
          DbAdminUtilities.loadFixtures()
      }
    }
  }
}
