/**
 * User: Björn Reimer
 * Date: 5/25/13
 * Time: 4:27 PM
 */

import play.api.GlobalSettings
import info.schleichardt.play.embed.mongo.DynamicEmbedMongoPort

object Global extends GlobalSettings with DynamicEmbedMongoPort {
  //replace "mongo.client.port" with your settings for your driver
  //i.e. for Reactive Mongo:
  import scala.collection.JavaConverters._
  override def additionalEmbedMongoPortSettings(port: Int) = Map("mongodb.servers" -> List(s"localhost:$port").asJava)
}
