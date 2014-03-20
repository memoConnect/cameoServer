package models

import java.util.Date
import traits.{ Model }
import play.api.libs.json._
import scala.concurrent.{ ExecutionContext, Future }
import ExecutionContext.Implicits.global
import reactivemongo.api.indexes.Index
import play.modules.reactivemongo.json.collection.JSONCollection
import helper.IdHelper
import helper.JsonHelper._
import reactivemongo.core.commands.LastError
import helper.MongoCollections

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 9:31 PM
 */
case class Token(id: MongoId,
                 created: Date) {

  def toJson: JsValue = Json.toJson(this)(Token.outputWrites)
}

object Token extends Model[Token] {

  val col = MongoCollections.identityCollection

  implicit val mongoFormat: Format[Token] = createMongoFormat(Json.reads[Token], Json.writes[Token])

  def docVersion = 0

  def evolutions = Map()

  def outputWrites = Writes[Token] {
    t =>
      Json.obj("token" -> t.id.toJson) ++
        addCreated(t.created)
  }

  override def createDefault(): Token = {
    new Token(
      IdHelper.generateAccessToken(),
      new Date)
  }

  override def save(js: JsObject): Future[LastError] = {
    val id: MongoId = (js \ "_id").as[MongoId]
    val query = arrayQuery("tokens", id)
    val set = Json.obj("$set" -> Json.obj("tokens.$" -> js))
    col.update(query, set)
  }

}
